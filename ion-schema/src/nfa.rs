use crate::ion_path::IonPath;
use crate::system::{TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::IonSchemaElement;
use ion_rs::value::owned::Element;
use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::rc::Rc;
use std::slice::Iter;

/// Represents an id for a state in NFA
type StateId = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Transition {
    // represents destination state for the transition
    destination: StateId,
    // represents the type_id for the destination state
    // this will be used to validate if an Ion value can be accepted at destination state or not
    type_id: TypeId,
    // minimum occurrence allowed for destination state
    // this will be used to verify if destination state is optional through check min == 0
    // and it will also be used when destination state is same as source state to verify minimum occurrence for the state
    min: usize,
    // maximum occurrence allowed for destination state
    max: usize,
}

impl Transition {
    /// Verify if the given Ion value is valid for the transition or not
    pub fn is_valid_for_ion_value(&self, element: &Element, type_store: &TypeStore) -> bool {
        let schema_element: IonSchemaElement = element.into();
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();

        match type_def.validate(&schema_element, type_store, &mut IonPath::default()) {
            Ok(_) => true,
            Err(violation) => false,
        }
    }

    /// Verifies if a destination state is optional state or not
    pub fn is_destination_state_optional(&self) -> bool {
        self.min == 0
    }

    /// Verifies if the minimum occurrence requirement is met for given visits count
    pub fn allows_exit_after_n_visits(&self, visits: usize) -> bool {
        self.min <= visits
    }

    /// Verifies if the maximum occurrence requirement is met for given visits count
    pub fn allows_n_visits(&self, visits: usize) -> bool {
        self.max >= visits
    }
}

/// Represents a final state of NFA
/// A final state is only reached if the visits to that state is between the (min, max) occurrence range
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FinalState {
    state_id: StateId,
    min: usize,
    max: usize,
}

impl FinalState {
    pub fn new(state_id: StateId, min: usize, max: usize) -> Self {
        Self { state_id, min, max }
    }
}

/// Represents an NFA that will eb used by the ordered_elements` constraint in order to validate an Ion Value
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Nfa {
    pub(crate) transitions: HashMap<StateId, HashSet<Transition>>, // represents transitions between states
    pub(crate) final_states: HashSet<FinalState>, // represents all the final states for NFA
}

impl Nfa {
    /// Provides all final states for the [Nfa]
    pub fn get_final_states(&self) -> HashSet<FinalState> {
        self.final_states.to_owned()
    }

    /// Provides all the possible transitions for given state
    pub fn get_transitions(&self, state_id: StateId) -> HashSet<Transition> {
        self.transitions
            .get(&state_id)
            .map(|s| s.to_owned())
            .unwrap_or(HashSet::new())
    }
}

/// A context that will be used to store state-visit counts.
/// This will be used by [NfaEvaluation] which uses [Nfa] to evaluate given Ion value.
/// With each element in the given ordered elements there will be a set of [NfaRun]s created for all possible transitions to next states.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NfaRun {
    state_id: StateId,
    state_visits: usize,
}

impl NfaRun {
    pub fn new(current_state_id: StateId, visits_to_current_state: usize) -> Self {
        Self {
            state_id: current_state_id,
            state_visits: visits_to_current_state,
        }
    }
}

/// This is a context which will be used while validating an Ion value for `ordered_elements` constraint using its NFA.
/// It stores a set of [NfaRun]s that changes for each element in the ordered elements. i.e. `visits` keeps changing for each element.
/// The final set of `visits` stored in [NfaEvaluation] will be used to determine if we reached the final state or not.
#[derive(Debug, Clone)]
pub struct NfaEvaluation {
    pub(crate) visits: HashSet<NfaRun>,
    pub(crate) nfa: Rc<Nfa>,
}

impl NfaEvaluation {
    pub fn new(nfa: Rc<Nfa>) -> Self {
        Self {
            visits: {
                let mut visits = HashSet::new();
                if !nfa.get_final_states().is_empty() {
                    visits.insert(NfaRun::new(0, 0));
                }
                visits
            },
            nfa,
        }
    }

    /// Verify if referenced [Nfa] for this [NfaEvaluation] has final state in the set of `visits` with correct visit count.
    pub fn has_final_state(&self, type_store: &TypeStore) -> bool {
        // If the `ordered_elements` had no final states meaning if `ordered_elements` constraint was empty,
        // then verify that `visits` is also empty for validation
        if self.nfa.get_final_states().is_empty() && self.visits.is_empty() {
            return true;
        }

        // verify if `visits` contains a final state in it with visit count between (min, max) for that fianl state
        self.visits.iter().any(|nfa_run| {
            self.nfa.get_final_states().iter().any(|fs| {
                fs.state_id == nfa_run.state_id
                    && nfa_run.state_visits >= fs.min
                    && nfa_run.state_visits <= fs.max
            })
        })
    }

    /// Validates provided ordered elements against referenced [Nfa]
    pub fn validate_ordered_elements(&mut self, elements: &[Element], type_store: &TypeStore) {
        let mut elements_iter = elements.iter().peekable();
        // given elements are actually events for the `Nfa` referenced in this `NfaEvaluation`.
        // iterate through all elements and update state-visit count(`NfaRun`) for all possible transitions for given element(event).
        while let Some(element) = elements_iter.next() {
            let mut next_states = HashSet::new();
            for nfa_run in self.visits.iter() {
                let transitions: HashSet<Transition> = self.nfa.get_transitions(nfa_run.state_id);
                // evaluate all possible transitions for nfa_run
                self.evaluate_transitions(
                    nfa_run,
                    transitions,
                    element,
                    &mut elements_iter,
                    type_store,
                    &mut next_states,
                );
            }
            self.visits = next_states;
        }
    }

    /// Evaluates given transitions using referenced [Nfa]
    /// For each evaluation of a transition it adds next possible states into `next_states`
    fn evaluate_transitions(
        &self,
        nfa_run: &NfaRun,
        transitions: HashSet<Transition>,
        current_element: &Element,
        elements: &mut Peekable<Iter<Element>>,
        type_store: &TypeStore,
        next_states: &mut HashSet<NfaRun>,
    ) {
        let source_state_id = nfa_run.state_id;
        let visits = nfa_run.state_visits;

        // traverse all transitions for source state and verify if we can take the given transition
        // Each transition has 3 possibilities:
        // - Transition loops back to same state
        // - Transition moves to the next state
        // - Transition moves to optional state
        for transition in transitions {
            let destination_state_id = transition.destination;

            // transition which loops back to same state
            if destination_state_id == source_state_id {
                if !&self.evaluate_transition_to_self(
                    visits,
                    &transition,
                    current_element,
                    elements,
                    type_store,
                    next_states,
                ) {
                    // given ordered elements are invalid because it didn't satisfy required minimum occurrence constraint
                    return;
                }
            } else if transition.is_valid_for_ion_value(current_element, type_store) {
                // if transition is valid, add destination state to next states
                next_states.insert(NfaRun::new(destination_state_id, 1));
            }

            // transition to optional state
            // if destination state is optional then add transitions to next states skipping the optional state
            self.evaluate_transition_to_optional_state(
                visits,
                &transition,
                current_element,
                elements,
                type_store,
                next_states,
            );
        }
    }

    // This is a helper method that is used by `evaluate_transitions()` to resolve destination states that are optional
    // for optional destination states, add transitions to next states skipping the optional state
    fn evaluate_transition_to_optional_state(
        &self,
        visits: usize,
        transition: &Transition,
        element: &Element,
        elements: &mut Peekable<Iter<Element>>,
        type_store: &TypeStore,
        next_states: &mut HashSet<NfaRun>,
    ) {
        let mut destination_states_for_optional_state: HashSet<Transition> = HashSet::new();

        if transition.is_destination_state_optional() {
            let mut transitions = self.nfa.get_transitions(transition.destination);

            // skip the optional state itself
            transitions.remove(transition);

            destination_states_for_optional_state.extend(transitions);

            self.evaluate_transitions(
                &NfaRun::new(transition.destination, visits),
                destination_states_for_optional_state.to_owned(),
                element,
                elements,
                type_store,
                next_states,
            );
        }
    }

    // this sia  helper method used by `evaluate_transitions` to evaluate transitions that loops back to the same state
    // this method iterates through elements to satisfy minimum required occurrences for given transition
    // It will return false if an invalid Ion value is found which doesn't satisfy minimum occurrence requirement for given transition
    // Otherwise it will return true
    fn evaluate_transition_to_self(
        &self,
        visits: usize,
        transition: &Transition,
        element: &Element,
        elements: &mut Peekable<Iter<Element>>,
        type_store: &TypeStore,
        next_states: &mut HashSet<NfaRun>,
    ) -> bool {
        let mut visit_count = visits + 1;
        let mut element = element;
        if transition.allows_n_visits(visit_count) {
            if transition.is_valid_for_ion_value(element, type_store) {
                // if transition is valid, add destination state to next states
                next_states.insert(NfaRun::new(transition.destination, visit_count));
            }
            // iterate through elements for at least minimum n visits of the given transition
            while !transition.allows_exit_after_n_visits(visit_count) {
                element = match elements.next() {
                    None => {
                        // if the minimum required occurrences for given transition is not met,
                        // and if the elements iterator is empty then the given Ion value is invalid
                        return false;
                    }
                    Some(element) => element,
                };
                if transition.is_valid_for_ion_value(element, type_store) {
                    visit_count += 1;
                    // if transition is valid, add destination state to next states
                    next_states.insert(NfaRun::new(transition.destination, visit_count));
                }
            }
        }
        true
    }
}

/// Represents a builder for constructing NFA which will be used by `ordered_elements` constraint
pub struct NfaBuilder {
    nfa: Nfa,
}

impl NfaBuilder {
    pub fn new() -> NfaBuilder {
        NfaBuilder {
            nfa: Nfa::default(),
        }
    }

    pub fn build(mut self, final_states: HashSet<FinalState>) -> Nfa {
        self.nfa.final_states = final_states;
        self.nfa
    }

    pub fn with_transition(
        &mut self,
        start_id: StateId,
        end_id: StateId,
        type_id: TypeId,
        min: usize,
        max: usize,
    ) {
        let end_states = self
            .nfa
            .transitions
            .entry(start_id)
            .or_insert_with(HashSet::new);

        end_states.insert(Transition {
            destination: end_id,
            type_id,
            min,
            max,
        });
    }
}
