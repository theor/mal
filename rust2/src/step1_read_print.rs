extern crate rustyline;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate itertools;

mod ast;
mod reader;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
  // `()` can be used when no completer is required
  let mut rl = Editor::<()>::new();
  if rl.load_history(".mal-history").is_err() {
    println!("No previous history.");
  }

  loop {
    let readline = rl.readline("user> ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(&line);
        rl.save_history(".mal-history").unwrap();
        if line.len() > 0 {
          let ast = reader::read_str(&line);
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
