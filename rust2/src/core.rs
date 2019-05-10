use crate::ast::Ast::*;
use crate::ast::{Ast, MalRes, MalErr, MalArgs, DisplayNonReadably};
use itertools::Itertools;

fn int_op(op: fn(i64, i64) -> Ast, a: &MalArgs) -> MalRes {
  match (a[0].clone(), a[1].clone()) {
    (Int(a0), Int(a1)) => Ok(op(a0, a1)),
    _ => Err(MalErr::ErrString("invalid int_op args".to_owned())),
  }
}

fn eq(a: &Ast, b: &Ast) -> MalRes {
  match (a, b) {
    (List(ref a), List(ref b)) | (Vector(ref a), Vector(ref b)) | (List(ref a), Vector(ref b)) | (Vector(ref a), List(ref b)) => {
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

pub fn ns() -> Vec<(&'static str, Ast)> {
    vec![
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
      Fun(|args, _env| match args[0] {
        List(_) => Ok(Bool(true)),
        _ => Ok(Bool(false)),
      }),
    ),
    // (
    //   "prn",
    //   Fun(|args, _env| {
    //     for arg in args.get(0).iter() {
    //       println!("{}", &arg);
    //     }
    //     Ok(Nil)
    //   }),
    // ),
    (
      "empty?",
      Fun(|args, _env| match &args[0] {
        List(ref l) | Vector(ref l) => Ok(Bool(l.len() == 0)),
        _ => Ok(Bool(false)),
      }),
    ),
    (
      "count",
      Fun(|args, _env| match &args[0] {
        List(ref l) | Vector(ref l) => Ok(Int(l.len() as i64)),
        _ => Ok(Int(0)),
      }),
    ),
    (
      "=",
      Fun(|args, _env| eq(&args[0], &args[1])),
    ),
    (
      "pr-str",
      Fun(|args, _env| Ok(MalString(format!("{}", args.iter().join(" ")))))
    ),
    (
      "str",
      Fun(|args, _env| Ok(MalString(format!("{}", args.iter().map(DisplayNonReadably).join("")))))
    ),
    (
      "prn",
      Fun(|args, _env| { println!("{}", args.iter().join(" ")); Ok(Nil)})
    ),
    (
      "println",
      Fun(|args, _env| { println!("{}",format!("{}", args.iter().map(DisplayNonReadably).join(" "))); Ok(Nil)})
    )
  ]
}