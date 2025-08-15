use std::{
    env,
    io::{self, Write},
    fs
};
use crate::parser::{Parser, Ast};
use crate::interpreter::{Interpreter, Value};

mod tokenizer;
mod parser;
mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        return spawn_shell();
    }

    let contents =  fs::read_to_string(&args[1]).expect("File not found.");
    let tokens = tokenizer::scan(contents);

    let ast = Parser::new(tokens).parse();
    // dbg!(ast);
    let mut interp = Interpreter::new(ast);
    let _values = interp.interpret();
}

fn spawn_shell() {
    println!("Welcome to the nova shell! v0.1.");

    // Persistent interpreter to keep environment across lines
    let mut interp = Interpreter::new(Ast { nodes: vec![] });

    loop {
        let mut line = String::new();

        print!(">>> ");

        io::stdout().flush().unwrap(); // Make sure the prompt appears immediately
        let bytes_read = io::stdin().read_line(&mut line).unwrap();

        if bytes_read == 0 || line.trim() == "exit" {
            println!();
            break;
        }

        let tokens = tokenizer::scan(line);
        let ast = Parser::new(tokens).parse();
        let values = interp.interpret_with(ast);
        for v in values {
            if v != Value::Nil {
                println!("{}", v);
            }
        }
    }
}
