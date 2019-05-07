use crate::ast::*;
use std::cell::RefCell;

#[derive(Clone)]
pub struct Env {
    outer: Option<Box<Env>>,
    map: RefCell<std::collections::hash_map::HashMap<String, Ast>>
}

impl Env {
    pub fn new() -> Self {
        Env {
            outer: None,
            map: Default::default(),
        }
    }

    pub fn from(parent: Box<Env>) -> Self {
        Self { outer: Some(parent), .. Self::new() }
    }

    pub fn insert(&mut self, key: &String, value: Ast) -> MalRes {
        self.map.borrow_mut().insert(key.clone(), value.clone());
        Ok(value)
    }

    pub fn get(&mut self, key: &String) -> Option<Ast> {
        let outer_ref = self.outer.as_mut();
        let b = self.map.borrow();
        b.get(key).map(|x| x.clone()).or_else(|| outer_ref.and_then(|parent| parent.get(key).clone()))
    }
}

// use std::ops::Deref;


// impl Deref for Env {
//     type Target = std::collections::hash_map::HashMap<String, Ast>;
//     fn deref(&self) -> &Self::Target {
//         &self.map
//     }
// }

// pub type Env = ;