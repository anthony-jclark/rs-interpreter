
#[derive(Debug, Clone)]
enum Token {
    Integer(i32),
    Star, Slash,
    Plus, Minus,
    LParen, RParen
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
            '('         => Token::LParen,
            ')'         => Token::RParen,
             c          => panic!("Error: unexpected character: '{}'.", c),
        };
        self.advance();
        Some(token)
    }
}

#[derive(Debug)]
enum AST {
    BinOp { lhs: Box<AST>, op: Token, rhs: Box<AST> },
    Num (i32),
}

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
}

impl Parser {
    fn new(source: String) -> Parser {
        let mut l = Lexer::new(source);
        let t = l.get_next_token();
        Parser {
            lexer: l,
            current_token: t,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.get_next_token();
    }

    fn factor(&mut self) -> AST {
        let ast = match self.current_token {
            Some(Token::Integer(i)) => AST::Num(i),
            Some(Token::LParen) => {
                self.advance();
                let paren_ast = self.expression();
                match self.current_token {
                    Some(Token::RParen) => paren_ast,
                    ref t => panic!("Error: expected an RParen found a {:?}", t),
                }
            }
            ref t => panic!("Error: expected an integer and found a '{:?}'.", t),
        };
        self.advance();
        ast
    }

    fn term(&mut self) -> AST {
        let mut ast = self.factor();

        loop {
            match self.current_token {
                Some(Token::Star)  => (),
                Some(Token::Slash) => (),
                _ => break,
            }
            let token = self.current_token.clone().unwrap();
            self.advance();
            ast = AST::BinOp {
                lhs: Box::new(ast), op: token, rhs: Box::new(self.factor())
            };
        }
        ast
    }

    fn expression(&mut self) -> AST {
        let mut ast = self.term();

        loop {
            match self.current_token {
                Some(Token::Plus)  => (),
                Some(Token::Minus) => (),
                _ => break,
            }
            let token = self.current_token.clone().unwrap();
            self.advance();
            ast = AST::BinOp {
                lhs: Box::new(ast), op: token, rhs: Box::new(self.term())
            };
        }
        ast
    }
}

#[derive(Debug)]
struct Interpreter {
    parser: Parser
}

impl Interpreter {
    fn new(source: String) -> Interpreter {
        Interpreter { parser: Parser::new(source) }
    }

    fn visit(&self, ast: AST) -> i32 {
        match ast {
            AST::BinOp{lhs, op, rhs} => match op {
                Token::Plus  => self.visit(*lhs) + self.visit(*rhs),
                Token::Minus => self.visit(*lhs) - self.visit(*rhs),
                Token::Star  => self.visit(*lhs) * self.visit(*rhs),
                Token::Slash => self.visit(*lhs) / self.visit(*rhs),
                _ => panic!("Error: expected arithmetic operator and found '{:?}'.", op),
            },
            AST::Num(i) => i,
        }
    }

    fn interpret(&mut self) -> i32 {
        let ast = self.parser.expression();
        self.visit(ast)
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
        let result = interpreter.interpret();
        println!("{:#?}", result);
        prompt!("calc> ");
    }
}
