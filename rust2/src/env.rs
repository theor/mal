use std::rc::Rc;
use crate::ast::*;
use std::cell::RefCell;
use std::fmt;

// #[derive(Debug)]
pub struct EnvStruct {
    pub outer: Option<Env>,
    map: RefCell<std::collections::hash_map::HashMap<String, Ast>>
}

#[derive(Clone)]
pub struct Env(pub Rc<EnvStruct>);

impl Env {
     pub fn new() -> Self {
         Env(Rc::new(EnvStruct {
            outer: None,
            map: Default::default(),
        }))
     }

    pub fn env_bind(parent: &Env) -> Self {
        // println!("env from {}", parent);
        let mut res = Self::new();
        Rc::get_mut(&mut res.0).unwrap().outer = Some(parent.clone());
        res
    }

    pub fn insert(&mut self, key: &String, value: Ast) -> MalRes {
        self.0.map.borrow_mut().insert(key.clone(), value.clone());
        Ok(value)
    }

    pub fn get(&self, key: &String) -> Option<Ast> {
        // println!("  get {} {}", key, self);
        let outer_ref = &self.0.outer.as_ref();
        let b = self.0.map.borrow();
        b.get(key).map(|x| x.clone()).or_else(|| outer_ref.and_then(|parent| parent.get(key).clone()))
    }
}

impl fmt::Display for EnvStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.map.borrow().keys())
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.as_ref().map.borrow().keys())
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