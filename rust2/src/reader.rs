use std::collections::HashMap;
use crate::ast::{MalErr, MalRes, Ast};
use regex::{Captures, Regex};

lazy_static! {
    static ref RE: Regex =
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###)
            .unwrap();
}

#[derive(Debug, Clone)]
struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    fn next(&mut self) -> Result<String, MalErr> {
        self.pos = self.pos + 1;
        Ok(self
            .tokens
            .get(self.pos - 1)
            .ok_or(MalErr::ErrString("EOF".to_string()))?
            .to_string())
    }
    fn peek(&self) -> Result<String, MalErr> {
        Ok(self
            .tokens
            .get(self.pos)
            .ok_or(MalErr::ErrString("EOF".to_string()))?
            .to_string())
    }
}

// impl Iterator for Reader {
//     type Item = String;

//     fn next(&mut self) -> Option<<Self as Iterator>::Item> {
//         unimplemented!()
//     }
// }

impl Reader {
    pub fn new(tokens: Vec<String>) -> Self {
        Reader { tokens, pos: 0 }
    }
}

fn read_list<F>(s: &mut Reader, end: char, f:F) -> MalRes
where F: FnOnce(Vec<Ast>) -> Ast {
    let mut res = Vec::new();
    loop {
        let next = s.peek()?;
        if next.chars().next().map_or(false, |c| c == end) {
            s.next()?;
            break;
        } else {
            let item = read_form(s)?;
            res.push(item)
        }
    }
    Ok(f(res))
}

fn unescape_str(s: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"\\(.)"#).unwrap();
    }
    RE.replace_all(&s, |caps: &Captures| {
        format!("{}", if &caps[1] == "n" { "\n" } else { &caps[1] })
    })
    .to_string()
}

fn read_atom(s: &mut Reader) -> MalRes {
    let n = s.next()?;
    match &n[..] {
        "nil" => Ok(Ast::Nil),
        "true" => Ok(Ast::Bool(true)),
        "false" => Ok(Ast::Bool(false)),
        _ => {
            if let Some('\"') = n.chars().next() {
                if n.chars().last() != Some('\"') {
                    Err(MalErr::ErrString("EOF".to_string()))
                } else {
                    Ok(Ast::MalString(unescape_str(&n[1..n.len() - 1]))) 
                }
            } else {
                n.parse::<i64>()
                    .map(|u| Ast::Int(u))
                    .or_else(|_| Ok(Ast::Sym(n)))
            }
        }
    }
}

fn read_form(s: &mut Reader) -> MalRes {
    let next = s.peek()?;
    match &next[..] {
        "@" => {
            s.next()?;
            let derefed = read_form(s)?;
            Ok(Ast::List(vec![Ast::Sym("deref".to_owned()), derefed]))
        },
        "(" => {
            s.next()?;
            read_list(s, ')', Ast::List)
        }
        "[" => {
            s.next()?;
            read_list(s, ']', Ast::Vector)
        }
        "{" => {
            s.next()?;
            // Ast::HashMap(std::default::Default::default())
            read_list(s, '}', |v| {
                let mut map: HashMap<String, Ast> = Default::default();
                let mut it = v.iter();
                while let Some(key) = it.next() {
                    let value = it.next().unwrap();
                    let key = match key {
                        Ast::MalString(ref s) | Ast::Sym(ref s) => s,
                        _ => panic!(""),
                    };
                    let _ = map.insert(key.to_string(), value.clone());
                }
                Ast::HashMap(map)
            })
        }
        _ => read_atom(s),
    }
}

pub fn read_str(s: &String) -> MalRes {
    let t = tokenize(s);
    let mut reader = Reader::new(t);
    read_form(&mut reader)
}

pub fn tokenize(s: &String) -> Vec<String> {
    let mut res = vec![];
    for cap in RE.captures_iter(s) {
        if cap[1].starts_with(";") {
            continue;
        }
        res.push(String::from(&cap[1]));
    }
    res
}
