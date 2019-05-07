use std::fmt;


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
pub type Env = std::collections::hash_map::HashMap<String, Ast>;
#[derive(Clone)]
pub enum Ast {
    List(Vec<Ast>),
    Vector(Vec<Ast>),
    Int(i64),
    Sym(String),
    MalString(String),
    Bool(bool),
    Nil,
    Fun(fn(&MalArgs) -> MalRes),
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

impl Ast {
    fn pr_str(&self, readably: bool, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Ast::*;
        match self {
            &Int(i) => write!(f, "{}", i),
            &List(ref l) => print_seq(readably, f, l, '(', ')'),
            &Vector(ref l) => print_seq(readably, f, l, '[', ']'),
            &Sym(ref s) => write!(f, "{}", s),
            &MalString(ref s) => {
                if readably {
                    write!(f, "\"{}\"", escape_str(s))
                } else {
                    write!(f, "{}", s)
                }
            }
            &Nil => write!(f, "nil"),
            &Bool(b) => write!(f, "{}", if b { "true" } else { "false" }),
            Fun(_) => write!(f, "fun"), // TODO ?
        }
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pr_str(true, f)
    }
}
