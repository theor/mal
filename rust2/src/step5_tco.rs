extern crate rustyline;
#[macro_use]
extern crate lazy_static;
extern crate itertools;
extern crate regex;

mod ast;
mod core;
mod env;
mod reader;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use ast::*;
use env::Env;

fn eval_ast(ast: &Ast, env: &mut Env) -> MalRes {
  // println!("eval ast {}", ast);
  use ast::Ast::*;
  match ast {
    Sym(ref s) => {
      if let Some(':') = s.chars().next() { // keyword
        Ok(ast.clone())
      } else if let Some(sym) = env.get(s) {
        Ok(sym.clone())
      } else {
        error_s(format!("{} not found", s).to_owned())
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
    HashMap(ref m) => m
      .iter()
      .map(|(k, v)| eval(v, env).map(|v2| (k.to_owned(), v2)))
      .collect::<Result<std::collections::HashMap<String, Ast>, MalErr>>()
      // .collect::<std::collections::HashMap<String, Result<Ast, MalErr>>>()
      .map(HashMap),
    _ => Ok(ast.clone()),
  }
}

fn error(s: &str) -> MalRes {
  Err(MalErr::ErrString(s.to_owned()))
}

fn error_s(s: String) -> MalRes {
  error(&s.to_owned())
}

fn call(call_list: &Vec<Ast>, env: &mut Env, ast: &mut Ast) -> Option<MalRes> {
  use ast::Ast::*;

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
    _ => Some(Err(MalErr::ErrString(format!(
      "expected a function, got {}",
      fun
    )))),
  }
}

fn eval(ast: &Ast, env: &mut Env) -> MalRes {
  // println!("eval {}", ast);
  use ast::Ast::*;
  let mut ast = ast.clone();
  let mut env = env.clone(); //Env(env.0);
  'tco: loop {
    match ast {
      List(ref l) => {
        if l.len() == 0 {
          return Ok(List(Vec::new()));
        } else {
          match &l[0] {
            Sym(ref s) if s == "let*" => {
              let tuples = &l[1];
              let scope = &l[2];
              let mut let_env = Env::env_bind(&env);

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
              env = let_env;
              ast = scope.clone();
              continue 'tco;
            }

            Sym(ref s) if s == "def!" => {
              // println!("ARG0 {}", &l[1]);
              // println!("pre def {}", env);
              match &l[1] {
                MalString(ref s) | Sym(ref s) => {
                  let value = eval(&l[2], &mut env)?;
                  let a = env.insert(s, value);
                  // println!("post def {}", env);
                  return a;
                }
                _ => unimplemented!("{}", &l[1]),
              }
            }

            Sym(ref s) if s == "do" => match l[1..]
              .iter()
              .try_fold(Nil, |_prev, cur| eval(cur, &mut env))?
            {
              x => { 
                ast = x;
                continue 'tco;
              }
              // List(_) => {
                // ast = l.iter().last().unwrap_or(&Nil).clone();
                // continue 'tco;
              // }
              // x => return error_s(&format!("invalid do form {}", x)),
            },
            Sym(ref s) if s == "if" => match eval(&l[1], &mut env)? {
              Nil | Bool(false) => {
                ast = l.get(3).unwrap_or(&Nil).clone();
                continue 'tco;
              }
              _ => {
                ast = l[2].clone();
                continue 'tco;
              }
            },
            Sym(ref s) if s == "fn*" => {
              // (fn* (a b) (+ b a))
              let args = match &l[1] {
                List(ref args) | Vector(ref args) => Ok(
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
              return Ok(MalFun {
                eval: eval,
                ast: std::rc::Rc::new(ast.clone()),
                params: args,
                env: Env::env_bind(&env),
              });
              // Err(MalErr::ErrString("fn not implemented".to_owned()))
            }
            _ => {
              let evaluated = eval_ast(&ast, &mut env)?;
              match evaluated {
                List(ref list) => {
                  let mut ast2 = ast.clone();
                  if let Some(res) = call(list, &mut env, &mut ast2) {
                    return res;
                  } else {
                    ast = ast2.clone();
                    continue 'tco;
                  }
                }
                _ => unreachable!(),
              }
            }
          }
        }
      }
      _ => return eval_ast(&ast, &mut env),
    }
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

  reader::read_str(&"(def! not (fn* (a) (if a false true)))".to_owned())
    .and_then(|x| eval(&x, &mut env))
    .unwrap();

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
