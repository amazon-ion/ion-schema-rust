use std::fmt::Debug;

// TODO: Fill the trait
pub trait Import: Debug + Clone {}

// TODO: Fill the struct
#[derive(Debug, Clone)]
pub struct ImportImpl{}

impl Import for ImportImpl {}