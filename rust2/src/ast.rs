use std::fmt;

#[derive(Clone, Debug)]
pub enum Ast {
    List(Vec<Ast>),
    Int(i32),
    Sym(String),
    MalString(String),
    Bool(bool),
    Nil,
    
}

fn escape_str(s: &str) -> String {
  s.chars().map(|c| {
    match c {
      '"' => "\\\"".to_string(),
      '\n' => "\\n".to_string(),
      '\\' => "\\\\".to_string(),
      _ => c.to_string(),
    }
  }).collect::<Vec<String>>().join("")
}

impl Ast {
    fn pr_str(&self, readably: bool, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Ast::*;
        match self {
            &Int(i) => write!(f, "{}", i),
            &List(ref l) => {
                let len = l.len();
                write!(f, "(")?;

                for (i, x) in l.iter().enumerate() {
                    x.pr_str(readably, f)?;
                    if i < len - 1 { write!(f, " ")?; }
                }
                write!(f, ")")
            },
            &Sym(ref s) => write!(f, "{}", s),
            &MalString(ref s) => if readably { write!(f, "\"{}\"", escape_str(s)) } else { write!(f, "{}", s) },
            &Nil => write!(f, "nil"),
            &Bool(b) => write!(f, "{}", if b { "true" } else { "false" }),
        }
    }

}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pr_str(true, f)
    }
}