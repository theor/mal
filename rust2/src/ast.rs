use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
 use std::cell::RefCell;
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
        is_macro: bool,
    },
    Atom(Rc<RefCell<Ast>>),
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
            Atom(x) => {
                write!(f, "(atom ")?;
                x.borrow().pr_str(readably, f)?;
                write!(f, ")")
            },
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

pub fn apply(call_list: &[Ast], env: &mut Env) -> MalRes {
  use Ast::*;
//   use itertools::Itertools;
//   println!("  apply {}", call_list.iter().join(" "));
  let fun = call_list.get(0).expect("list should not be empty");
  let args = &call_list[1..];
   match fun {
    Fun(f) => f(args, env),
    MalFun {
      ast: fun_ast,
      eval,
      params,
      env: ref closure_env,
      is_macro: _,
    } => {
      let mut call_env = Env::env_bind(closure_env);
      let mut params_it = params.iter();
      let mut args_it = args.iter();
      while let Some(p) = params_it.next() {
        match p.as_ref() {
          "&" => {
            let variadic_name = params_it.next().expect("variadic params name missing");
            let variadic_list = Ast::List(args_it.cloned().collect::<Vec<Ast>>());
            // println!("variadic {} {}", variadic_name, variadic_list);
            call_env.insert(variadic_name, variadic_list).unwrap();
            break;
          },
          _ => { /*println!(" arg {}", p);*/ call_env.insert(p, args_it.next().unwrap().clone()).unwrap(); },
        }
      }
      
      eval(fun_ast, &mut call_env)
    }
    _ => {
      // panic!("apply {}", fun);
      Err(MalErr::ErrString(format!(
      "expected a function, got {}",
      fun
    )))
    },
  }
}


pub fn call(call_list: &[Ast], env: &mut Env, ast: &mut Ast) -> Option<MalRes> {
  use Ast::*;

  // println!("  call {}", Ast::List(call_list.clone()));

  let fun = call_list.get(0).expect("list should not be empty");
  let args = &call_list[1..];
  match fun {
    Fun(f) => Some(f(args, env)),
    MalFun {
      ast: fun_ast,
      eval: _,
      params,
      env: ref closure_env,
      is_macro: _,
    } => {
      let mut call_env = Env::env_bind(closure_env);
      let mut params_it = params.iter();
      let mut args_it = args.iter();
      while let Some(p) = params_it.next() {
        match p.as_ref() {
          "&" => {
            let variadic_name = params_it.next().expect("variadic params name missing");
            let variadic_list = Ast::List(args_it.cloned().collect::<Vec<Ast>>());
            // println!("variadic {} {}", variadic_name, variadic_list);
            call_env.insert(variadic_name, variadic_list).unwrap();
            break;
          },
          _ => { call_env.insert(p, args_it.next().unwrap().clone()).unwrap(); },
        }
      }
      
      *env = call_env;
      *ast = fun_ast.as_ref().clone();
      None
      // eval(ast, &mut call_env)
    }
    _ => { 
      // panic!("call {}", fun);
      Some(Err(MalErr::ErrString(format!(
      "expected a function, got {}",
      fun
    ))))
    },
  }
}

