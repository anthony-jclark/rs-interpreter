//! This is part 1 of Ruslan Spivak's Let’s Build A Simple Interpreter series.


#[derive(Debug)]
enum Token {
    Integer(i32),
    Plus,
    Minus,
    EOF
}


#[derive(Debug)]
struct Interpreter {
    text: Vec<char>,
    position: usize,
}


impl Interpreter {
    fn new(source: String) -> Interpreter {
        Interpreter {
            text: source.chars().collect(),
            position: 0,
        }
    }

    ///
    /// Advance our position index
    ///
    fn advance(&mut self) {
        self.position += 1;
    }

    ///
    /// Check for the end of out input text
    ///
    fn done(&self) -> bool {
        self.text.len() == 0 || self.position > self.text.len() - 1
    }

    ///
    /// Evaluate expressions of the form:
    ///     Integer Plus Integer
    ///     Integer Minus Integer
    ///
    fn expression(&mut self) -> i32 {
        use ::Token::*;

        let lhs = match self.get_next_token() {
            Integer(i) => i,
            t => panic!("Error: expected an integer and found a '{:?}'", t),
        };
        let op = match self.get_next_token() {
            t @ Plus | t @ Minus => t,
            t => panic!("Error: expected an operator and found a '{:?}'", t),
        };
        let rhs = match self.get_next_token() {
            Integer(i) => i,
            t => panic!("Error: expected an integer and found a '{:?}'", t),
        };
        match op {
            Plus => lhs + rhs,
            Minus => lhs - rhs,
            _ => panic!("Should never reach here."),
        }
    }

    ///
    /// Tokenize the input.
    /// It accepts integers and the '+' and '-' signs.
    ///
    fn get_next_token(&mut self) -> Token {
        self.skip_ws();

        if self.done() {
            return Token::EOF;
        }

        let token = match self.text[self.position] {
            '0' ... '9' => self.get_integer(),
            '+' => Token::Plus,
            '-' => Token::Minus,
            c   => panic!("Error: unexpected character: {}.", c),
        };
        self.advance();
        token
    }

    ///
    /// Create a (potentially) multi-digit integer
    ///
    fn get_integer(&mut self) -> Token {
        let mut number = self.text[self.position].to_string();
        while let Some(c) = self.peek() {
            match c {
                '0' ... '9' => number.push(c),
                _ => break,
            }
            self.advance();
        }
        Token::Integer(number.parse::<i32>().unwrap())
    }

    ///
    /// Return the current character indexed by position. Note:
    /// this returns an option since we might be done with the
    /// source text.
    ///
    fn look(&self) -> Option<char> {
        if !self.done() {
            Some(self.text[self.position])
        } else {
            None
        }
    }

    ///
    /// Look at the next character. Note: this returns an option
    /// since we could potentially look past the end of the source
    /// text.
    ///
    fn peek(&self) -> Option<char> {
        if !self.done() && self.position + 1 < self.text.len() {
            Some(self.text[self.position + 1])
        } else {
            None
        }
    }

    ///
    /// Advance past any whitespace characters. This function does
    /// not use look or peek so that I can avoid unwrapping the options.
    ///
    fn skip_ws(&mut self) {
        while !self.done() && self.text[self.position].is_whitespace() {
            self.advance();
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
