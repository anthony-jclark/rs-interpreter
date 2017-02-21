//! This is part 1 of Ruslan Spivak's Let’s Build A Simple Interpreter series.


#[derive(Debug)]
enum Token {
    Integer(u32),
    Plus,
    EOF
}


#[derive(Debug)]
struct Interpreter {
    text: Vec<char>,
    position: usize,
    current_token: Token,
}


impl Interpreter {
    fn new(source: String) -> Interpreter {
        Interpreter {
            text: source.chars().collect(),
            position: 0,
            current_token: Token::EOF,
        }
    }

    ///
    /// Tokenize the input.
    /// It only accepts single digit integers and the '+' sign.
    ///
    fn get_next_token(&mut self) -> Token {
        if self.position > self.text.len() - 1 {
            return Token::EOF;
        }

        let token = match self.text[self.position] {
            d @ '0' ... '9' => Token::Integer(d.to_digit(10).unwrap()),
            '+' => Token::Plus,
            c => panic!("Error: unexpected character: {}.", c),
        };
        self.position += 1;
        token
    }

    ///
    /// Evaluate expressions of the form:
    ///     Integer Plus Integer
    ///
    fn expression(&mut self) -> u32 {
        use ::Token::*;

        let lhs = self.get_next_token();
        let op  = self.get_next_token();
        let rhs = self.get_next_token();
        match (lhs, op, rhs) {
            (Integer(li), Plus, Integer(ri)) => li + ri,
            (t1, t2, t3) => panic!("Error: invalid input string: {:?} {:?} {:?}.", t1, t2, t3),
        }
    }

}

macro_rules! prompt(
    ($($arg:tt)*) => { {
        print!($($arg)*);
        stdout().flush().expect("Could not flush stdout");
    } }
);

fn main() {
    use std::io::{BufRead, stdin, stdout, Write};

    prompt!("calc> ");
    let stdin = stdin();
    for line in stdin.lock().lines() {
        let mut interpreter = Interpreter::new(line.unwrap());
        let result = interpreter.expression();
        println!("{}", result);
        prompt!("calc> ");
    }
}
