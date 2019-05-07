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

fn call(call_list: &Vec<Ast>, _env: &mut Env) -> MalRes {
  use ast::Ast::*;

  let fun = call_list.get(0).expect("list should not be empty");
  let args = &call_list[1..];
  match fun {
    Fun(f) => f(args),
    _ => unreachable!(),
  }
}
fn eval(ast: &Ast, env: &mut Env) -> MalRes {
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
            let mut let_env =  Env::from(Box::new(env.clone()));
            
            let mut tuples_it = match tuples { 
              List(ref tuples) | Vector(ref tuples) => tuples.iter(),
               _ => unreachable!("tuples must be a list, was {}", tuples), };
            while let Some(name) = tuples_it.next() {
              let let_name = match name { MalString(ref s) | Sym(ref s) => s, _ => unreachable!("let binding is not a name") };
              let value = tuples_it.next().expect("bound name has no value");
              let value = eval(value, &mut let_env)?;
              let_env.insert(let_name, value).unwrap();
            }
            eval(scope, &mut let_env)
          },
          Sym(ref s) if s == "def!" => {
            // println!("ARG0 {}", &l[1]);
            match &l[1] {
              MalString(ref s) | Sym(ref s) => {
                let value = eval(&l[2], env)?;
                env.insert(s, value)
              }
              _ => unimplemented!("{}", &l[1]),
            }
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

fn int_op(op: fn(i64, i64) -> i64, a: &MalArgs) -> MalRes {
  use ast::Ast::Int;
  match (a[0].clone(), a[1].clone()) {
    (Int(a0), Int(a1)) => Ok(Int(op(a0, a1))),
    _ => Err(MalErr::ErrString("invalid int_op args".to_owned())),
  }
}

fn main() {
  // `()` can be used when no completer is required
  let mut rl = Editor::<()>::new();
  if rl.load_history(".mal-history").is_err() {
    println!("No previous history.");
  }

  let mut env = Env::new();
  env
    .insert(
      &"+".into(),
      ast::Ast::Fun(|args| int_op(|i, j| i + j, args)),
    )
    .unwrap();
  env
    .insert(
      &"-".into(),
      ast::Ast::Fun(|args| int_op(|i, j| i - j, args)),
    )
    .unwrap();
  env
    .insert(
      &"/".into(),
      ast::Ast::Fun(|args| int_op(|i, j| i / j, args)),
    )
    .unwrap();
  env
    .insert(
      &"*".into(),
      ast::Ast::Fun(|args| int_op(|i, j| i * j, args)),
    )
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
