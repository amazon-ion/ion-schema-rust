use crate::ion_path::IonPath;
use crate::system::{TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::IonSchemaElement;
use ion_rs::value::owned::Element;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Represents an event in NFA which can either be an Ion Value or an end of stream
#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Value(Element),
    EndOfStream,
}

impl Event {
    // Note: can not use this as a From<Element> implementation as Element is not defined within this crate
    pub(crate) fn from_elements_to_events<T: IntoIterator<Item = Element>>(iter: T) -> Vec<Event> {
        let mut events: Vec<Event> = iter
            .into_iter()
            .map(<Element as Into<Event>>::into)
            .collect();
        // add END_OF_STREAM at the end of events vec
        events.push(Event::EndOfStream);
        events
    }
}

impl From<Element> for Event {
    fn from(value: Element) -> Self {
        Event::Value(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Transition {
    destination: usize,      // represents destination state
    to_optional_state: bool, // represents if destination state is an optional state or not
    type_id: Option<TypeId>, // None is used for final state transition
}

/// Represents an NFA that will eb used by the ordered_elements` constraint in order to validate an Ion Value
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Nfa {
    pub(crate) states: Vec<State>, // represents states with state_id
    pub(crate) transitions: HashMap<usize, HashSet<Transition>>, // represents transitions between states
}

/// This is a context which will be used while validating an Ion value for `ordered_elements` constraint using its NFA
#[derive(Debug, Clone)]
pub struct NfaRun {
    pub(crate) visits: HashMap<usize, usize>, // represents a map of (stat_id, visit_count)
    pub(crate) nfa: Rc<Nfa>,                  // reference to the NFA
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

    pub fn transition(&mut self, event: Event, type_store: &TypeStore) -> HashMap<usize, usize> {
        let mut new_states = HashMap::new();
        for (source_state_id, visits) in self.visits.iter() {
            let can_exit = self.nfa.states[*source_state_id].allows_exit_after_n_visits(*visits);
            let transitions: &HashSet<Transition> =
                self.nfa.transitions.get(source_state_id).unwrap();
            self.evaluate_transitions(
                *source_state_id,
                transitions,
                can_exit,
                *visits,
                event.to_owned(),
                type_store,
                &mut new_states,
            );
        }
        new_states
    }

    fn evaluate_transitions(
        &self,
        source_state_id: usize,
        transitions: &HashSet<Transition>,
        can_exit: bool,
        visits: usize,
        event: Event,
        type_store: &TypeStore,
        new_states: &mut HashMap<usize, usize>,
    ) {
        let mut destination_states_for_optional_state: HashSet<Transition> = HashSet::new();
        for transition in transitions {
            let destination_state_id = transition.destination;
            let destination_state = &self.nfa.states[destination_state_id];

            if destination_state_id == source_state_id {
                if self.nfa.states[destination_state_id].allows_n_visits(visits + 1) {
                    let can_enter = destination_state.allows_to_enter_for_ion_value(
                        event.to_owned(),
                        transition.type_id,
                        type_store,
                    );
                    if can_enter {
                        new_states.insert(destination_state_id, visits + 1);
                    }
                }
            } else if can_exit {
                let can_enter = destination_state.allows_to_enter_for_ion_value(
                    event.to_owned(),
                    transition.type_id,
                    type_store,
                );
                if can_enter {
                    new_states.insert(destination_state_id, 1);
                }
            }

            // for optional state add transitions to next states skipping the optional state
            if transition.to_optional_state {
                let mut transitions = self
                    .nfa
                    .transitions
                    .get(&destination_state_id)
                    .unwrap()
                    .to_owned();

                // skip the optional state itself
                transitions.remove(transition);

                destination_states_for_optional_state.extend(transitions);

                self.evaluate_transitions(
                    destination_state_id,
                    &destination_states_for_optional_state,
                    can_exit,
                    visits,
                    event.to_owned(),
                    type_store,
                    new_states,
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

    pub fn build(self) -> Nfa {
        self.nfa
    }

    pub fn with_state(&mut self, state: State) {
        self.nfa.states.push(state);
    }

    pub fn total_states(&self) -> usize {
        self.nfa.states.len()
    }

    pub fn with_transition(
        &mut self,
        start_id: usize,
        end_id: usize,
        type_id: Option<TypeId>,
        to_optional_state: bool,
    ) {
        let end_states = self
            .nfa
            .transitions
            .entry(start_id)
            .or_insert_with(HashSet::new);

        end_states.insert(Transition {
            destination: end_id,
            to_optional_state,
            type_id,
        });
    }
}

// The NFA contains 3 types of states:
// * Initial - This state simply represents an initial state to start building the NFA. (Only one Initial state is allowed)
// * Intermediate - This state represents all the intermediate states for NFA. Each intermediate state contains a (min, max) range which represents occurrence for an event to reach that state.
// * Final - This state represents the final state which is only reached if the validation succeeds (Only one Final state is allowed)
#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Initial,
    Intermediate { min: usize, max: usize },
    Final,
}

impl State {
    pub fn allows_to_enter_for_ion_value(
        &self,
        event: Event,
        type_id: Option<TypeId>,
        type_store: &TypeStore,
    ) -> bool {
        match &self {
            State::Initial => false,
            State::Intermediate { min, max } => {
                let schema_element: IonSchemaElement = match event {
                    Event::Value(element) => (&element).into(),
                    Event::EndOfStream => {
                        return false;
                    }
                };

                // type definition validation for given event
                let type_def = type_store.get_type_by_id(type_id.unwrap()).unwrap();

                match type_def.validate(&schema_element, type_store, &mut IonPath::default()) {
                    Ok(_) => true,
                    Err(violation) => false,
                }
            }
            State::Final => event == Event::EndOfStream,
        }
    }

    pub fn allows_exit_after_n_visits(&self, visits: usize) -> bool {
        match self {
            State::Initial => true,
            State::Intermediate { min, .. } => *min <= visits,
            State::Final => false,
        }
    }

    pub fn allows_n_visits(&self, visits: usize) -> bool {
        match self {
            State::Initial => false,
            State::Intermediate { max, .. } => *max >= visits,
            State::Final => false,
        }
    }
}
