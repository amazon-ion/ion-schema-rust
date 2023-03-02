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
    destination: StateId, // represents destination state
    type_id: TypeId,
    min: usize, // minimum occurrence allowed for destination state
    max: usize, // maximum occurrence allowed for destination state
}

impl Transition {
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

    pub fn allows_exit_after_n_visits(&self, visits: usize) -> bool {
        self.min <= visits
    }

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
    pub fn get_final_states(&self) -> HashSet<FinalState> {
        self.final_states.to_owned()
    }

    pub fn get_transitions(&self, state_id: StateId) -> HashSet<Transition> {
        self.transitions
            .get(&state_id)
            .map(|s| s.to_owned())
            .unwrap_or(HashSet::new())
    }
}

/// A context that will be used for a single transition
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

/// This is a context which will be used while validating an Ion value for `ordered_elements` constraint using its NFA
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

    pub fn has_final_state(&self, type_store: &TypeStore) -> bool {
        if self.nfa.get_final_states().is_empty() && self.visits.is_empty() {
            return true;
        }

        self.visits.iter().any(|nfa_run| {
            self.nfa.get_final_states().iter().any(|fs| {
                fs.state_id == nfa_run.state_id
                    && nfa_run.state_visits >= fs.min
                    && nfa_run.state_visits <= fs.max
            })
        })
    }

    pub fn validate_ordered_elements(&mut self, elements: Vec<Element>, type_store: &TypeStore) {
        let mut elements_iter = elements.iter().peekable();
        // use nfa_evaluation for validation
        while let Some(element) = elements_iter.next() {
            let mut next_states = HashSet::new();
            for nfa_run in self.visits.iter() {
                let transitions: HashSet<Transition> = self.nfa.get_transitions(nfa_run.state_id);
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

    fn evaluate_transitions(
        &self,
        nfa_run: &NfaRun,
        transitions: HashSet<Transition>,
        element: &Element,
        elements: &mut Peekable<Iter<Element>>,
        type_store: &TypeStore,
        next_states: &mut HashSet<NfaRun>,
    ) {
        let mut element = element;
        let mut destination_states_for_optional_state: HashSet<Transition> = HashSet::new();

        let source_state_id = nfa_run.state_id;
        let visits = nfa_run.state_visits;

        // traverse all transitions for source state and verify if we can take the given transition
        for transition in transitions {
            let destination_state_id = transition.destination;

            if destination_state_id == source_state_id {
                let mut visit_count = visits + 1;
                if transition.allows_n_visits(visit_count) {
                    if transition.is_valid_for_ion_value(element, type_store) {
                        // if transition is valid, add destination state to next states
                        next_states.insert(NfaRun::new(destination_state_id, visit_count));
                    }
                    // consume elements for at least minimum n visits
                    while !transition.allows_exit_after_n_visits(visit_count) {
                        element = match elements.next() {
                            None => return,
                            Some(element) => element,
                        };
                        if transition.is_valid_for_ion_value(element, type_store) {
                            visit_count += 1;
                            // if transition is valid, add destination state to next states
                            next_states.insert(NfaRun::new(destination_state_id, visit_count));
                        }
                    }
                }
            } else if transition.is_valid_for_ion_value(element, type_store) {
                // if transition is valid, add destination state to next states
                next_states.insert(NfaRun::new(destination_state_id, 1));
            }

            // if destination state is optional then add transitions to next states skipping the optional state
            if transition.is_destination_state_optional() {
                let mut transitions = self.nfa.get_transitions(destination_state_id);

                // skip the optional state itself
                transitions.remove(&transition);

                destination_states_for_optional_state.extend(transitions);

                self.evaluate_transitions(
                    &NfaRun::new(destination_state_id, visits),
                    destination_states_for_optional_state.to_owned(),
                    element,
                    elements,
                    type_store,
                    next_states,
                );
            }
        }
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
