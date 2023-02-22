use crate::ion_path::IonPath;
use crate::system::{TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::IonSchemaElement;
use ion_rs::value::owned::Element;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type Event = Element;
pub(crate) const END_OF_STREAM_EVENT: Option<Event> = None;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Transition {
    destination: usize,
    type_id: Option<TypeId>, // None is used for final state transition
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Nfa {
    states: Vec<State>,
    transitions: HashMap<usize, HashSet<Transition>>,
}

#[derive(Debug, Clone)]
pub struct NfaRun {
    pub(crate) visits: HashMap<usize, usize>,
    nfa: Rc<Nfa>,
}

impl NfaRun {
    pub fn new(nfa: Rc<Nfa>) -> Self {
        Self {
            visits: {
                let mut visits = HashMap::new();
                visits.insert(0, 1);
                visits
            },
            nfa,
        }
    }

    pub fn has_final_state(&self) -> bool {
        self.visits
            .iter()
            .any(|(id, _)| self.nfa.states[*id] == State::Final)
    }

    pub fn transition(
        &mut self,
        event: Option<Event>,
        type_store: &TypeStore,
    ) -> HashMap<usize, usize> {
        let mut new_states = HashMap::new();
        for (transition_from_state_id, visits) in self.visits.iter() {
            let can_exit = self.nfa.states[*transition_from_state_id].can_exit(visits);
            let transition_to_states: &HashSet<Transition> =
                self.nfa.transitions.get(transition_from_state_id).unwrap();
            for transition_to_state in transition_to_states {
                let transition_to_state_id = transition_to_state.destination;
                if transition_to_state_id == *transition_from_state_id {
                    if self.nfa.states[transition_to_state_id].can_reenter(&(visits + 1)) {
                        let can_enter = self.nfa.states[transition_to_state_id].can_enter(
                            event.to_owned(),
                            transition_to_state.type_id,
                            type_store,
                        );
                        if can_enter {
                            new_states.insert(transition_to_state_id, visits + 1);
                        }
                    }
                } else if can_exit {
                    let can_enter = self.nfa.states[transition_to_state_id].can_enter(
                        event.to_owned(),
                        transition_to_state.type_id,
                        type_store,
                    );
                    if can_enter {
                        new_states.insert(transition_to_state_id, 1);
                    }
                }
            }
        }
        new_states
    }
}

pub struct NfaBuilder {
    nfa: Nfa,
}

impl NfaBuilder {
    pub fn new() -> NfaBuilder {
        NfaBuilder {
            nfa: Nfa::default(),
        }
    }

    pub fn build(self) -> Nfa {
        self.nfa
    }

    pub fn with_state(&mut self, state: State) {
        self.nfa.states.push(state);
    }

    pub fn total_states(&self) -> usize {
        self.nfa.states.len()
    }

    pub fn with_transition(&mut self, start_id: usize, end_id: usize, type_id: Option<TypeId>) {
        let end_states = self
            .nfa
            .transitions
            .entry(start_id)
            .or_insert_with(HashSet::new);

        end_states.insert(Transition {
            destination: end_id,
            type_id,
        });
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Initial,
    Intermediate { min: usize, max: usize },
    Final,
}

impl State {
    pub fn can_enter(
        &self,
        event: Option<Event>,
        type_id: Option<TypeId>,
        type_store: &TypeStore,
    ) -> bool {
        match &self {
            State::Initial => false,
            State::Intermediate { min, max } => {
                if event.is_none() {
                    return false;
                }
                let schema_element: IonSchemaElement = (&event.unwrap()).into();

                // type definition validation for given event
                let type_def = type_store.get_type_by_id(type_id.unwrap()).unwrap();

                match type_def.validate(&schema_element, type_store, &mut IonPath::default()) {
                    Ok(_) => true,
                    Err(violation) => false,
                }
            }
            State::Final => event == END_OF_STREAM_EVENT,
        }
    }

    pub fn can_exit(&self, visits: &usize) -> bool {
        match &self {
            State::Initial => true,
            State::Intermediate { min, .. } => min <= visits,
            State::Final => false,
        }
    }

    pub fn can_reenter(&self, visits: &usize) -> bool {
        match &self {
            State::Initial => false,
            State::Intermediate { max, .. } => max >= visits,
            State::Final => false,
        }
    }
}
