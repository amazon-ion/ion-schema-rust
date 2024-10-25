//! Implementation of a Non-deterministic Finite-state Automaton for the `ordered_elements` constraint.
//!
//! The general idea is this:
//!
//! > The NFA consumes a sequence of input events, one by one. In each step, whenever two or more
//! > transitions are applicable, it "clones" itself into appropriately many copies, each one
//! > following a different transition. If exactly one transition is applicable, it follows that
//! > transition without making any copies. If no transition is applicable, the current copy is in a
//! > dead end, and it "dies". If, after consuming the complete input, any of the copies is in an
//! > accept state, the input is accepted, else, it is rejected. [...]
//! >
//! > Keep a set data structure of all states which the NFA might currently be in. On the
//! > consumption of an input event, unite the results of the transition function applied to all
//! > current states to get the set of next states. [...]
//! >
//! > On the consumption of the last input event, if one of the current states is a final state,
//! > the machine accepts the sequence.
//!
//! (Paraphrased from [Nondeterministic_finite_automaton](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton))
//!
//! Strictly speaking, our implementation might not be properly finite. For an `ordered_elements`
//! member such as `{ type: int, occurs: range::[0, max] }`, there are infinite possible states
//! since it could accept a sequence of integers that is any length. We avoid having an
//! infinite-sized graph by constructing one state for each member in the `ordered_elements`
//! constraint and storing the min and max possible visits for each state.
//!
//! As we are traversing the state machine graph, we track the current state of the machine as a set
//! of `(state_id, visit_count)` pairs. For something that accepts a theoretically infinite input
//! sequence (as above), this could result in an infinite number of parallel states, but in practice
//! the number of possible states is bounded by the length of the input.

use crate::ion_path::{IonPath, IonPathElement};
use crate::result::ValidationResult;
use crate::system::TypeStore;
use crate::type_reference::{TypeReference, VariablyOccurringTypeRef};
use crate::types::TypeValidator;
use crate::violation::{Violation, ViolationCode};
use crate::IonSchemaElement;
use ion_rs::Element;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::swap;
use std::ops::RangeInclusive;
use std::vec;

/// Unique identifier for a particular node/state in the NFA.
///
/// Each [State] has an integer identifier ([StateId]) which must be unique within any
/// [OrderedElementsNfa] instance. This identifier is used for the implementation of traits such as
/// [Ord], [Eq], and  [Hash] for types such as [TraversalError].
type StateId = usize;

/// In the evaluation of the NFA, used for tracking which states are in the set of possible current states.
type StateVisitCount = (StateId, usize);

/// Represents an event in the input sequence—it is either some [Element] or the end-of-sequence
/// marker (i.e. [Option::None]).
type ElementOrEndOfSequence<'a> = Option<&'a Element>;

/// The compiled state machine for an `ordered_elements` constraint.
///
/// This is represented as a directed, almost a-cyclical graph. The only cycles allowed are
/// loops—i.e. an edge that is connected to the same vertex at both ends.
#[derive(Debug, Clone, PartialEq)]
pub struct OrderedElementsNfa {
    /// The vertices/nodes of the state machine.
    states: Vec<State>,
    /// An adjacency list describing the directed edges between `states`.
    ///
    /// Because the `ordered_elements` constraint does not have any sub-sequence grouping, all
    /// outgoing edges must have consecutively numbered destinations—so we can model the edges as a
    /// map of `from_id` (the index in the vec) to a `Range` of `StateId`.
    edges: Vec<RangeInclusive<usize>>,
    /// Stores the [StateId] of the final state (packaged into a [StateVisitCount]) for convenience.
    /// When running the state machine, if the set of current states contains this `StateVisitCount`,
    /// we are done evaluating, and the input was accepted by the state machine.
    terminal_state: StateVisitCount,
}

impl OrderedElementsNfa {
    /// Constructs an [OrderedElementsNfa] from a [Vec] of pairs of [VariablyOccurringTypeRef] and
    /// an optional string description.
    ///
    /// The description is a human friendly string that goes into the violation messages to describe
    /// which entry in the `ordered_elements` constraint is producing the violation. If a
    /// description is provided, it should be something that is recognizable to users, such as a
    /// row/col in the source ISL or a snippet of the source ISL.
    /// If no description is provided, the default is `<ELEMENT[i]>`, where `i` is the index in the
    /// `ordered_element` constraint's list of variably occurring type references.
    pub fn new(intermediate_states: Vec<(VariablyOccurringTypeRef, Option<String>)>) -> Self {
        // "Initial" state is always first—no surprise there.
        let mut states = vec![State::Initial];

        // Construct intermediate states and add to our vec of states.
        intermediate_states
            .into_iter()
            .enumerate()
            .for_each(|(i, (var_type_ref, description))| {
                let description =
                    description.unwrap_or_else(|| format!("<ORDERED_ELEMENT[{}]>", i));
                let (min_visits, max_visits) = var_type_ref.occurs_range().inclusive_endpoints();
                let state = IntermediateState {
                    type_ref: var_type_ref.type_ref(),
                    min_visits,
                    max_visits,
                    description,
                };

                states.push(State::Intermediate(i + 1, state))
            });

        // This will become the ID of the "Final" state, but it's convenient to wait to add the
        // "Final" state until we've determined the edges of the graph.
        let max_id = states.len();

        // Construct an adjacency list to represent the edges of the graph.
        let mut edges = vec![];
        for (i, s) in states.iter().enumerate() {
            // Loop back to self, if max is > 1
            let min_transition = if s.can_reenter(1) { i } else { i + 1 };

            // Add transitions forward up to (including) the first type with a min occurs that is greater than 0
            let mut j = i + 1;
            while j < max_id {
                if !states[j].can_exit(0) {
                    break;
                }
                j += 1;
            }
            edges.push(min_transition..=j)
        }

        states.push(State::Final(max_id));

        // Terminal state is the Final state with a visit count of 1.
        let terminal_state: StateVisitCount = (max_id, 1usize);

        OrderedElementsNfa {
            states,
            edges,
            terminal_state,
        }
    }

    /// Tests an input sequence of [Element]
    pub fn matches<'a, I: Iterator<Item = &'a Element>>(
        &self,
        mut iter: I,
        type_store: &'a TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut current_state_set: HashSet<StateVisitCount> = HashSet::new();
        let mut new_states: HashSet<StateVisitCount> = HashSet::new();

        let mut input_index = 0;
        current_state_set.insert((0usize, 1usize));

        // Essentially, for-each input, but we want to capture the `Option::None` at the end of the iterator.
        loop {
            let element: ElementOrEndOfSequence = iter.next();
            let mut invalid_transitions: HashSet<TraversalError> = HashSet::new();

            ion_path.push(IonPathElement::Index(input_index));

            // For each state in the set of current states...
            for &(from_state_id, num_visits) in &current_state_set {
                let from_state = &self.states[from_state_id];

                let edges = if let Some(edges) = self.edges.get(from_state_id) {
                    // Need to clone the range because strangely &RangeInclusive doesn't
                    // implement Copy or IntoIterator.
                    edges.clone()
                } else {
                    // The only state without edges is `Final`, which cannot be exited.
                    invalid_transitions.insert(TraversalError::CannotExitState(from_state_id));
                    break;
                };

                // For each edge out of the current state we are inspecting...
                for to_state_id in edges {
                    let to_state: &State = &self.states[to_state_id];

                    let can_reenter = from_state.can_reenter(num_visits);
                    let can_exit = from_state.can_exit(num_visits);
                    let is_loop = to_state_id == from_state_id;

                    if !is_loop && !can_exit {
                        invalid_transitions.insert(TraversalError::CannotExitState(from_state_id));
                        // We haven't reached the min_occurs of the current state. Any further
                        // transitions will also suffer from the same problem. No need to report
                        // this same problem repeatedly, so we break here.
                        break;
                    }

                    // TODO: Consider caching the result of this so that *if* there are multiple
                    //       current states that could transition to the same state, we don't end
                    //       up doing the same work multiple times.
                    let can_enter = to_state.can_enter(element, type_store, ion_path);

                    if let Err(violation) = can_enter {
                        invalid_transitions
                            .insert(TraversalError::CannotEnterState(to_state_id, violation));
                    } else if is_loop && !can_reenter {
                        invalid_transitions.insert(TraversalError::CannotReEnterState(to_state_id));
                    } else {
                        let new_num_visits = if is_loop { num_visits + 1 } else { 1 };
                        new_states.insert((to_state_id, new_num_visits));
                    }
                }
            }

            // There are no valid paths to continue through the graph.
            if new_states.is_empty() {
                return Err(self.build_violation(element, ion_path, invalid_transitions));
            }

            ion_path.pop();

            if new_states.contains(&self.terminal_state) {
                return Ok(());
            }

            current_state_set.clear();
            swap(&mut current_state_set, &mut new_states);
            input_index += 1;
        }
    }

    /// Build a [Violation] out of the set of [TraversalError]s.
    fn build_violation(
        &self,
        event: ElementOrEndOfSequence,
        ion_path: &mut IonPath,
        invalid_transitions: HashSet<TraversalError>,
    ) -> Violation {
        let mut reasons: Vec<_> = invalid_transitions.into_iter().collect();
        reasons.sort();
        let reasons = reasons
            .into_iter()
            .map(|it| match it {
                TraversalError::CannotExitState(s) => Violation::new(
                    "ordered_elements",
                    ViolationCode::ElementMismatched,
                    format!("{}: min occurs not reached", &self.states[s]),
                    ion_path,
                ),
                TraversalError::CannotReEnterState(s) => Violation::new(
                    "ordered_elements",
                    ViolationCode::ElementMismatched,
                    format!("{}: max occurs already reached", &self.states[s],),
                    ion_path,
                ),
                TraversalError::CannotEnterState(s, v) => Violation::with_violations(
                    "ordered_elements",
                    ViolationCode::ElementMismatched,
                    format!("{}: does not match type", &self.states[s]),
                    ion_path,
                    vec![v],
                ),
            })
            .collect();

        let index = ion_path.pop().unwrap();

        Violation::with_violations(
            "ordered_elements",
            ViolationCode::ElementMismatched,
            format!(
                "input does not match ordered_elements at index {}: {}",
                index,
                event
                    .map(Element::to_string)
                    .unwrap_or_else(|| "<END_OF_INPUT>".to_string())
            ),
            ion_path,
            reasons,
        )
    }
}

/// Details for a state that represents one of the [VariablyOccurringTypeReferences] in an
/// `ordered_elements` constraint.
#[derive(Debug, Clone, PartialEq)]
struct IntermediateState {
    type_ref: TypeReference,
    min_visits: usize,
    max_visits: usize,
    description: String,
}

/// Represents a state in the compiled nondeterministic finite automaton.
#[derive(Debug, Clone, PartialEq)]
enum State {
    Initial,
    Intermediate(StateId, IntermediateState),
    Final(StateId),
}

impl State {
    /// The unique integer identifier for this state.
    fn id(&self) -> StateId {
        match self {
            State::Initial => 0usize,
            State::Intermediate(id, _) => *id,
            State::Final(id) => *id,
        }
    }

    /// Checks whether the state can be visited more times or if the current path must move to a
    /// different state.
    fn can_reenter(&self, num_visits: usize) -> bool {
        match self {
            State::Initial => false,
            State::Intermediate(_, s) => num_visits < s.max_visits,
            State::Final(_) => false,
        }
    }

    /// Checks whether the state has been visited enough times to allow exiting that state (as
    /// opposed to either looping or dying out).
    fn can_exit(&self, num_visits: usize) -> bool {
        match self {
            State::Initial => true,
            State::Intermediate(_, s) => num_visits >= s.min_visits,
            State::Final(_) => false,
        }
    }

    /// Tests whether the `element` is valid for the [TypeReference] of this state.
    fn can_enter(
        &self,
        element: Option<&Element>,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        match self {
            State::Initial => unreachable!("There are no transitions to the initial state."),
            State::Intermediate(_, s) => {
                if let Some(el) = element {
                    let t = s.type_ref;
                    t.validate(&IonSchemaElement::from(el), type_store, ion_path)
                } else {
                    Err(Violation::new(
                        "ordered_elements",
                        ViolationCode::ElementMismatched,
                        "expected another element; found <END OF SEQUENCE>",
                        ion_path,
                    ))
                }
            }
            State::Final(_) => {
                if element.is_some() {
                    Err(Violation::new(
                        "ordered_elements",
                        ViolationCode::ElementMismatched,
                        format!("expected <END OF SEQUENCE>; found: {}", element.unwrap()),
                        ion_path,
                    ))
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            State::Initial => "<START OF SEQUENCE>",
            State::Intermediate(i, s) => &s.description,
            State::Final(_) => "<END OF SEQUENCE>",
        };
        f.write_str(string)
    }
}

/// The reason why a transition (or edge) in the state machine graph cannot be traversed.
#[derive(Debug)]
enum TraversalError {
    CannotEnterState(StateId, Violation),
    CannotExitState(StateId),
    CannotReEnterState(StateId),
}

impl PartialOrd for TraversalError {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TraversalError {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_id = match self {
            TraversalError::CannotEnterState(id, _)
            | TraversalError::CannotExitState(id)
            | TraversalError::CannotReEnterState(id) => id,
        };
        let other_id = match other {
            TraversalError::CannotEnterState(id, _)
            | TraversalError::CannotExitState(id)
            | TraversalError::CannotReEnterState(id) => id,
        };
        self_id.cmp(other_id)
    }
}

impl Eq for TraversalError {}

impl PartialEq for TraversalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                TraversalError::CannotExitState(self_id),
                TraversalError::CannotExitState(other_id),
            ) => self_id == other_id,
            (
                TraversalError::CannotReEnterState(self_id),
                TraversalError::CannotReEnterState(other_id),
            ) => self_id == other_id,
            // It is okay to ignore the violation here because we only consider one event/element at
            // any given point in the state machine. Since that is the case, if the IDs are the same,
            // then they must represent the same destination state (type reference), and so the
            // violations must be equal.
            (
                TraversalError::CannotEnterState(self_id, _),
                TraversalError::CannotEnterState(other_id, _),
            ) => self_id == other_id,
            (_, _) => false,
        }
    }
}

impl Hash for TraversalError {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // By using unique primes, we cannot get a hash collision unless there's at least as many
        // states as the smallest of the prime numbers. Furthermore, the relatively large spacing
        // between the prime numbers makes it even more unlikely that a collision would occur since
        // the first IDs that could have a collision with each other would be 107 and 307.
        state.write_usize(match self {
            TraversalError::CannotEnterState(id, _) => id * 503,
            TraversalError::CannotExitState(id) => id * 307,
            TraversalError::CannotReEnterState(id) => id * 107,
        })
    }
}
