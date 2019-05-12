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

fn get_seq(a: &Ast) -> Option<&Vec<Ast>> {
  match a {
    Ast::List(ref l) | Ast::Vector(ref l) if l.len() > 0 => Some(l),
    _ => None,
  }
}

macro_rules! sym {
   ($to_match:expr) => { Ast::Sym($to_match.to_owned()) }
}

fn is_macro_call(ast: &Ast, env: &Env) -> Option<Vec<Ast>> {
  if let Some(seq) = get_seq(ast) {
  match &seq[0] {
    Ast::Sym(ref s) => {
      let val =  env.get(s)?;
      match val {
        Ast::MalFun { ref is_macro, .. } if *is_macro => Some(std::iter::once(val.clone()).chain(seq[1..].iter().cloned()).collect()),
        _ => None,
      }
    },
    _ => None,
  }
  } else {
    None
  }
}

fn macroexpand(ast: &Ast, env: &mut Env) -> MalRes {
  let mut ast = ast.clone();
  while let Some(macro_call_list) = is_macro_call(&ast, env) {
    // println!("    pre loop expand {}", ast);
    ast = crate::ast::apply(&macro_call_list, env)?;
    // println!("      loop expand {}", ast);
  }
  Ok(ast)
}

// macro_rules! list {
//    ($($var:ident,)*) => { Ast::List($to_match.to_owned()) }
// }

fn quasiquote(ast: &Ast) -> Ast {
  if let Some(l) = get_seq(ast) {
    match &l[0] {
      Ast::Sym(ref s) if s == "unquote" => l[1].clone(),
      x => { 
        if let Some(l2) = get_seq(x) {
          match &l2[0] {
            Ast::Sym(ref s2) if s2 == "splice-unquote" => return Ast::List(vec![sym!("concat"), l2[1].clone(), quasiquote(&Ast::List(l[1..].to_vec())) ]),
            _ => {},
          }
        }
        return Ast::List(vec![sym!("cons"), quasiquote(x), quasiquote(&Ast::List(l[1..].to_vec()))]);
      },
    }
  } else {
    Ast::List(vec![sym!("quote"), ast.clone()])
  }
}

fn eval(ast: &Ast, env: &mut Env) -> MalRes {
  // println!("eval {}", ast);
  use ast::Ast::*;
  let mut ast = ast.clone();
  let mut env = env.clone(); //Env(env.0);
  'tco: loop {
    // println!("pre expand {}", ast);
    ast = macroexpand(&ast, &mut env)?;
    // println!("  post {}", ast);
    match ast {
      List(ref l) => {
        if l.len() == 0 {
          return Ok(List(Vec::new()));
        } else {
          match &l[0] {
            Sym(ref s) if s == "macroexpand" => {
              return macroexpand(&l[1], &mut env)
            },
            Sym(ref s) if s == "quote" => {
              return Ok(l[1].clone())
            },
            Sym(ref s) if s == "quasiquote" => {
              ast = quasiquote(&l[1]);
              continue 'tco;
            },
            Sym(ref s) if s == "unquote" => {
              return error("niy")
            },
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

             Sym(ref s) if s == "defmacro!" => {
              // println!("ARG0 {}", &l[1]);
              // println!("pre def {}", env);
              match &l[1] {
                MalString(ref s) | Sym(ref s) => {
                  let value = eval(&l[2], &mut env)?;
                  let value = match value {
                   MalFun { eval,
ast,
params,
env,
is_macro: _, } => MalFun { is_macro: true, eval,
ast,
params,
env,
 },
                    _ => value,
                  };
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
                is_macro: false,
              });
              // Err(MalErr::ErrString("fn not implemented".to_owned()))
            },
            
            Sym(ref s) if s == "eval" => {
              // println!("eval {}", &l[1]);
              ast = eval(&l[1], &mut env)?;
              while let Some(ref e) = env.0.outer {
                env = e.clone();
              }
              continue 'tco;
            },
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

fn rep(s: &String, mut env: &mut Env) {
  if s == "#env" { println!("{}", env); }
  match reader::read_str(s)
    .and_then(|x| eval(&x, &mut env)) {
      Ok(ast) => println!("{}", ast),
      Err(e) => println!("{}", e),
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

  rep(&"(def! not (fn* (a) (if a false true)))".to_owned(), &mut env);
  rep(&"(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))".to_owned(), &mut env);
  rep(&"(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))".to_owned(), &mut env);
  rep(&"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))".to_owned(), &mut env);


  let mut args = std::env::args();
  args.next().unwrap(); // bin name
  if let Some(file) = args.next() {
    println!("{}", file);
    let argv = args.map(|a| Ast::MalString(a.to_owned())).collect::<Vec<Ast>>();
    env.insert(&"*ARGV*".to_owned(), Ast::List(argv)).unwrap();
    rep(&format!("(load-file \"{}\")", file), &mut env);
    return;
  } else {
    env.insert(&"*ARGV*".to_owned(), Ast::List(Vec::new())).unwrap();
  }

  loop {
    let readline = rl.readline("user> ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(&line);
        rl.save_history(".mal-history").unwrap();
        if line.len() > 0 {
          rep(&line, &mut env);
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
