
#[derive(Debug, PartialEq)]
enum Token {
    Integer(i32),
    Star, Slash,
    Plus, Minus
}

#[derive(Debug)]
struct Lexer {
    source: Vec<char>,
    position: usize,
}

impl Lexer {
    fn new(source: String) -> Lexer {
        Lexer {
            source: source.chars().collect(),
            position: 0,
        }
    }

    fn done(&self) -> bool {
        self.position >= self.source.len()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn string(&self, start: usize, end: usize) -> String {
        (&self.source[start..end]).to_vec().into_iter().collect()
    }

    fn next_char_is<P>(&self, pred: P) -> bool where P: Fn(char) -> bool {
        if self.position + 1 < self.source.len() {
            pred(self.source[self.position + 1])
        } else {
            false
        }
    }

    fn char(&mut self) -> Option<char> {
        if !self.done() {
            Some(self.source[self.position])
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.done() && self.source[self.position].is_whitespace() {
            self.advance();
        }
    }

    fn scan_integer(&mut self) -> Token {
        let start = self.position;
        while self.next_char_is(|c| c.is_digit(10)) {
            self.advance();
        }
        Token::Integer(self.string(start, self.position + 1).parse::<i32>().unwrap())
    }

    fn get_next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.done() { return None; }
        let token = match self.char().unwrap() {
            '0' ... '9' => self.scan_integer(),
            '+'         => Token::Plus,
            '-'         => Token::Minus,
            '*'         => Token::Star,
            '/'         => Token::Slash,
             c          => panic!("Error: unexpected character: '{}'.", c),
        };
        self.advance();
        Some(token)
    }
}


#[derive(Debug)]
struct Interpreter {
    lexer: Lexer,
    current_token: Option<Token>,
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

    fn advance(&mut self) {
        self.current_token = self.lexer.get_next_token();
    }

    fn factor(&mut self) -> i32 {
        let result = match self.current_token {
            Some(Token::Integer(i)) => i,
            ref t => panic!("Error: expected an integer and found a '{:?}'.", t),
        };
        self.advance();
        result
    }

    fn term(&mut self) -> i32 {
        let mut result = self.factor();

        loop {
            match self.current_token {
                Some(Token::Star)  => { self.advance(); result *= self.factor(); },
                Some(Token::Slash) => { self.advance(); result /= self.factor(); },
                _ => break,
            }
        }
        result
    }

    fn expression(&mut self) -> i32 {
        let mut result = self.term();

        loop {
            match self.current_token {
                Some(Token::Plus)  => { self.advance(); result += self.term() },
                Some(Token::Minus) => { self.advance(); result -= self.term() },
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
        let result = interpreter.expression();
        println!("{}", result);
        prompt!("calc> ");
    }
}
