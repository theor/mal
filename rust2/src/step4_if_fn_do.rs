extern crate rustyline;
#[macro_use]
extern crate lazy_static;
extern crate itertools;
extern crate regex;

mod ast;
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
    // HashMap(m) => l.iter().map(|x| eval(x, env)).collect::<Result<HashMap<String, Ast>, MalErr>>().map(Vector),
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
    _ => unreachable!("expected a function, got {}", fun),
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

fn int_op(op: fn(i64, i64) -> Ast, a: &MalArgs) -> MalRes {
  use ast::Ast::Int;
  match (a[0].clone(), a[1].clone()) {
    (Int(a0), Int(a1)) => Ok(op(a0, a1)),
    _ => Err(MalErr::ErrString("invalid int_op args".to_owned())),
  }
}

fn eq(a: &Ast, b: &Ast) -> MalRes {
  use ast::Ast::*;
  match (a, b) {
    (List(ref a), List(ref b)) | (Vector(ref a), Vector(ref b)) => {
      if a.len() != b.len() {
        Ok(Bool(false))
      } else {
        a.iter()
          .zip(b.iter())
          .try_fold(Bool(true), |_prev, (a, b)| eq(a, b))
      }
    }
    (Int(a), Int(b)) => Ok(Bool(a == b)),
    (Bool(a), Bool(b)) => Ok(Bool(a == b)),
    (Sym(ref a), Sym(ref b)) => Ok(Bool(a == b)),
    (MalString(ref a), MalString(ref b)) => Ok(Bool(a == b)),
    (Nil, Nil) => Ok(Bool(true)),
    // (Fun, Fun) => Ok(Bool(a == b)),
    // (MalFun, MalFun) => Ok(Bool(a == b)),
    _ => Ok(Bool(false)),
  }
}

fn main() {
  // `()` can be used when no completer is required

  let mut rl = Editor::<()>::new();
  if rl.load_history(".mal-history").is_err() {
    println!("No previous history.");
  }

  use ast::Ast::*;
  let ns = vec![
    ("+", Fun(|args, _env| int_op(|i, j| Int(i + j), args))),
    ("-", Fun(|args, _env| int_op(|i, j| Int(i - j), args))),
    ("/", Fun(|args, _env| int_op(|i, j| Int(i / j), args))),
    ("*", Fun(|args, _env| int_op(|i, j| Int(i * j), args))),
    ("<", Fun(|args, _env| int_op(|i, j| Bool(i < j), args))),
    (">", Fun(|args, _env| int_op(|i, j| Bool(i > j), args))),
    ("<=", Fun(|args, _env| int_op(|i, j| Bool(i <= j), args))),
    (">=", Fun(|args, _env| int_op(|i, j| Bool(i >= j), args))),
    (
      "list",
      Fun(|args, _env| Ok(List(args.iter().cloned().collect()))),
    ),
    (
      "list?",
      Fun(|args, env| match eval_ast(&args[0], env)? {
        List(_) => Ok(Bool(true)),
        _ => Ok(Bool(false)),
      }),
    ),
    (
      "prn",
      Fun(|args, _env| {
        for arg in args.get(0).iter() {
          println!("{}", &arg);
        }
        Ok(Nil)
      }),
    ),
    (
      "empty?",
      Fun(|args, env| match eval_ast(&args[0], env)? {
        List(ref l) => Ok(Bool(l.len() == 0)),
        _ => Ok(Bool(false)),
      }),
    ),
    (
      "count",
      Fun(|args, env| match eval_ast(&args[0], env)? {
        List(ref l) => Ok(Int(l.len() as i64)),
        _ => Ok(Int(0)),
      }),
    ),
    (
      "=",
      Fun(|args, env| eq(&eval_ast(&args[0], env)?, &eval_ast(&args[1], env)?)),
    ),
  ];

  let mut env = Env::new();
  for (n, v) in ns.into_iter() {
    env.insert(&n.into(), v).unwrap();
  }

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
