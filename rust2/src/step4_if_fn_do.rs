extern crate rustyline;
#[macro_use]
extern crate lazy_static;
extern crate itertools;
extern crate regex;

mod ast;
mod env;
mod reader;
mod core;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use ast::*;
use env::Env;

fn eval_ast(ast: &Ast, env: &mut Env) -> MalRes {
  // println!("eval ast {}", ast);
  use ast::Ast::*;
  match ast {
    Sym(ref s) => {
      if let Some(sym) = env.get(s) {
        Ok(sym.clone())
      } else {
        Err(MalErr::ErrString(format!("{} not found", s).to_owned()))
      }
    }
    List(ref l) => {
      if l.len() == 0 {
        Ok(List(Vec::new()))
      } else {
        let evaluated: Result<Vec<Ast>, MalErr> = l.iter().map(|x| eval(x, env)).collect();
        evaluated.map(List)
      }
    }
    Vector(ref l) => l
      .iter()
      .map(|x| eval(x, env))
      .collect::<Result<Vec<Ast>, MalErr>>()
      .map(Vector),
    HashMap(ref m) => m.iter().map(|(k,v)| eval(v, env).map(|v2| (k.to_owned(), v2)))
      .collect::<Result<std::collections::HashMap<String, Ast>, MalErr>>()
      // .collect::<std::collections::HashMap<String, Result<Ast, MalErr>>>()
      
      .map(HashMap)
      ,
    _ => Ok(ast.clone()),
  }
}

fn call(call_list: &Vec<Ast>, env: &mut Env) -> MalRes {
  use ast::Ast::*;

  // println!("  call {}", Ast::List(call_list.clone()));

  let fun = call_list.get(0).expect("list should not be empty");
  let args = &call_list[1..];
  match fun {
    Fun(f) => f(args, env),
    MalFun {
      ast,
      eval,
      params,
       env: ref closure_env,
    } => {
      let mut call_env = Env::env_bind(closure_env);
      for (p, arg) in params.iter().zip(args.iter()) {
        call_env.insert(p, arg.clone()).unwrap();
      }
      eval(ast, &mut call_env)
    }
    _ => Err(MalErr::ErrString(format!("expected a function, got {}", fun))),
  }
}

fn eval(ast: &Ast, env: &mut Env) -> MalRes {
  // println!("eval {}", ast);
  use ast::Ast::*;
  match ast {
    List(ref l) => {
      if l.len() == 0 {
        Ok(List(Vec::new()))
      } else {
        match &l[0] {
          Sym(ref s) if s == "let*" => {
            let tuples = &l[1];
            let scope = &l[2];
            let mut let_env = Env::env_bind(env);

            let mut tuples_it = match tuples {
              List(ref tuples) | Vector(ref tuples) => tuples.iter(),
              _ => unreachable!("tuples must be a list, was {}", tuples),
            };
            while let Some(name) = tuples_it.next() {
              let let_name = match name {
                MalString(ref s) | Sym(ref s) => s,
                _ => unreachable!("let binding is not a name"),
              };
              let value = tuples_it.next().expect("bound name has no value");
              let value = eval(value, &mut let_env)?;
              let_env.insert(let_name, value).unwrap();
            }
            eval(scope, &mut let_env)
          }

          Sym(ref s) if s == "def!" => {
            // println!("ARG0 {}", &l[1]);
            // println!("pre def {}", env);
            match &l[1] {
              MalString(ref s) | Sym(ref s) => {
                let value = eval(&l[2], env)?;
                let a = env.insert(s, value);
                // println!("post def {}", env);
                a
              }
              _ => unimplemented!("{}", &l[1]),
            }
          }

          Sym(ref s) if s == "do" => l[1..].iter().try_fold(Nil, |_prev, cur| eval(cur, env)),
          Sym(ref s) if s == "if" => match eval(&l[1], env)? {
            Nil | Bool(false) => l.get(3).map_or(Ok(Nil), |x| eval(x, env)),
            _ => eval(&l[2], env),
          },
          Sym(ref s) if s == "fn*" => {
            // (fn* (a b) (+ b a))
            let args = match &l[1] {
              List(ref args) => Ok(
                args
                  .iter()
                  .map(|a| match a {
                    Sym(ref sym) => sym.clone(),
                    _ => panic!("asdasd"),
                  })
                  .collect(),
              ),
              _ => Err(MalErr::ErrString("arguments must be a list".to_owned())),
            }?;
            let ast = &l[2];
            Ok(MalFun {
              eval: eval,
              ast: std::rc::Rc::new(ast.clone()),
              params: args,
              env: Env::env_bind(env),
            })
            // Err(MalErr::ErrString("fn not implemented".to_owned()))
          }
          _ => {
            let evaluated = eval_ast(ast, env)?;
            match evaluated {
              List(ref list) => call(list, env),
              _ => unreachable!(),
            }
          }
        }
      }
    }
    _ => eval_ast(ast, env),
  }
}

fn main() {
  // `()` can be used when no completer is required

  let mut rl = Editor::<()>::new();
  if rl.load_history(".mal-history").is_err() {
    println!("No previous history.");
  }

  let mut env = Env::new();
  for (n, v) in core::ns().into_iter() {
    env.insert(&n.into(), v).unwrap();
  }

  reader::read_str(&"(def! not (fn* (a) (if a false true)))".to_owned()).and_then(|x| eval(&x, &mut env)).unwrap();

  loop {
    let readline = rl.readline("user> ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(&line);
        rl.save_history(".mal-history").unwrap();
        if line.len() > 0 {
          let ast = reader::read_str(&line).and_then(|x| eval(&x, &mut env));
          match ast {
            Ok(ast) => println!("{}", ast),
            Err(e) => println!("{}", e),
          }
        }
      }
      Err(ReadlineError::Interrupted) => continue,
      Err(ReadlineError::Eof) => break,
      Err(err) => {
        println!("{}", err);
        break;
      }
    }
  }
}
