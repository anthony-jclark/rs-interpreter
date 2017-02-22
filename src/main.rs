//! This is part 1 of Ruslan Spivak's Let’s Build A Simple Interpreter series.


#[derive(Debug)]
enum Token {
    Integer(i32),
    Star,
    Slash,
    EOF
}

#[derive(Debug)]
struct Lexer {
    text: Vec<char>,
    position: usize,
    current_char: Option<char>
}

impl Lexer {
    fn new(source: String) -> Lexer {
        let s = source.chars().collect::<Vec<char>>();
        let c = if s.len() > 0 { Some(s[0]) } else { None };
        Lexer {
            text: s,
            position: 0,
            current_char: c
        }
    }

    fn done(&self) -> bool {
        self.position + 1 > self.text.len()
    }


    fn advance(&mut self) {
        self.position += 1;
        if self.position > self.text.len() - 1 {
            self.current_char = None;
        } else {
            self.current_char = Some(self.text[self.position]);
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.done() && self.current_char.unwrap().is_whitespace() {
            self.advance();
        }
    }

    fn scan_integer(&mut self) -> Token {
        let mut integer = String::new();
        while !self.done() && self.current_char.unwrap().is_digit(10) {
            integer.push(self.current_char.unwrap());
            self.advance();
        }
        Token::Integer(integer.parse::<i32>().unwrap())
    }

    fn get_next_token(&mut self) -> Token {
        self.skip_whitespace();
        if let Some(c) = self.current_char {
            match c {
                '0' ... '9' => self.scan_integer(),
                '*' => { self.advance(); Token::Star },
                '/' => { self.advance(); Token::Slash },
                 c  => panic!("Error: unexpected character: {}.", c),
            }
        } else {
            Token::EOF
        }
    }
}


#[derive(Debug)]
struct Interpreter {
    lexer: Lexer,
    current_token: Token,
}

impl Interpreter {
    fn new(source: String) -> Interpreter {
        let mut l = Lexer::new(source);
        let t = l.get_next_token();
        Interpreter {
            lexer: l,
            current_token: t,
        }
    }

    fn eat(&mut self) {
        self.current_token = self.lexer.get_next_token();
    }

    fn parse_factor(&mut self) -> i32 {
        match self.current_token {
            Token::Integer(i) => { self.eat(); i },
            ref t => panic!("Error: expected an integer and found a '{:?}'", t),
        }
    }

    fn parse_expression(&mut self) -> i32 {
        let mut result = self.parse_factor();

        loop {
            match self.current_token {
                Token::Star  => { self.eat(); result *= self.parse_factor() },
                Token::Slash => { self.eat(); result /= self.parse_factor() },
                _ => break,
            }
        }

        result
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
        let result = interpreter.parse_expression();
        println!("{}", result);
        prompt!("calc> ");
    }
}
