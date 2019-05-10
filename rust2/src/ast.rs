use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use crate::env::Env;

#[derive(Debug, Clone)]
pub enum MalErr {
    ErrString(String),
}

pub type MalRes = Result<Ast, MalErr>;
pub type MalArgs = [Ast];


impl fmt::Display for MalErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MalErr::ErrString(ref e) => write!(f, "{}", e),
        }
    }
}

// pub type MalFn = Fn(&[Ast]) -> Ast;
#[derive(Clone)]
pub enum Ast {
    List(Vec<Ast>),
    Vector(Vec<Ast>),
    HashMap(std::collections::HashMap<String,Ast>),
    Int(i64),
    Sym(String),
    MalString(String),
    Bool(bool),
    Nil,
    Fun(fn(&MalArgs, &mut Env) -> MalRes),
    MalFun {
        eval: fn(&Ast, &mut Env) -> MalRes,
        ast: Rc<Ast>,
        params: Vec<String>,
        env: Env,
    },
}

fn escape_str(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

fn print_seq(readably: bool, f: &mut fmt::Formatter<'_>, l: &Vec<Ast>, start: char, end: char) ->  fmt::Result {
    let len = l.len();
    write!(f, "{}", start)?;

    for (i, x) in l.iter().enumerate() {
        x.pr_str(readably, f)?;
        if i < len - 1 {
            write!(f, " ")?;
        }
    }
    write!(f, "{}", end)
}

fn print_hashmap(readably: bool, f: &mut fmt::Formatter<'_>, l: &HashMap<String, Ast>) ->  fmt::Result {
    let len = l.len();
    write!(f, "{{" )?;

    for (i, (k, v)) in l.iter().enumerate() {
        Ast::MalString(k.to_string()).pr_str(readably, f)?;
        write!(f, " ")?;
        v.pr_str(readably, f)?;
        if i < len - 1 {
            write!(f, " ")?;
        }
    }
    write!(f, "}}")
}

impl Ast {
    pub fn pr_str(&self, readably: bool, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Ast::*;
        match self {
            &Int(i) => write!(f, "{}", i),
            &List(ref l) => print_seq(readably, f, l, '(', ')'),
            &Vector(ref l) => print_seq(readably, f, l, '[', ']'),
            &HashMap(ref l) => print_hashmap(readably, f, l),
            &Sym(ref s) => write!(f, "{}", s),
            &MalString(ref s) => {
                if let Some(':') = s.chars().next() {
                    write!(f, "{}", s)
                } else if readably {
                    write!(f, "\"{}\"", escape_str(s))
                } else {
                    write!(f, "{}", s)
                }
            }
            &Nil => write!(f, "nil"),
            &Bool(b) => write!(f, "{}", if b { "true" } else { "false" }),
            Fun(_) => write!(f, "#<function>"), 
            MalFun { .. } =>  write!(f, "#<function>"),
        }
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pr_str(true, f)
    }
}

pub struct DisplayNonReadably<'a>(pub &'a Ast);

impl<'a> fmt::Display for DisplayNonReadably<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.pr_str(false, f)
    }
}

pub fn error(s: &str) -> MalRes {
  Err(MalErr::ErrString(s.to_owned()))
}

pub fn error_s(s: String) -> MalRes {
  error(&s.to_owned())
}
