///
/// program : compound_statement Dot
/// compound_statement : Begin statement_list End
/// statement_list : statement
///                | statement Semi statement_list
/// statement : compound_statement
///           | assignment_statement
///           | empty
/// assignment_statement : variable Assign expression
/// variable : Id
/// empty :
///
/// expression : term ((Plus | Minus) term)*
/// term       : factor ((Star | Slash) factor)*
/// factor     : (Plus | Minus) factor
///            | Integer
///            | LParen expression RParen
///            | variable
///

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Token {
    Integer(i32),
    Star, Slash,
    Plus, Minus,
    LParen, RParen,
    Begin, End,
    Dot, Semi, Assign,
    Id(String),
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

    fn integer(&mut self) -> Token {
        let start = self.position;
        while self.next_char_is(|c| c.is_digit(10)) {
            self.advance();
        }

        Token::Integer(self.string(start, self.position + 1).parse::<i32>().unwrap())
    }

    fn word(&mut self) -> Token {
        use std::ascii::AsciiExt;

        let start = self.position;
        while self.next_char_is(|c| c.is_alphanumeric() && c.is_ascii()) {
            self.advance();
        }

        let word = self.string(start, self.position + 1);
        match word.as_ref() {
            "BEGIN" => Token::Begin,
            "END"   => Token::End,
            _       => Token::Id(word),
        }
    }

    fn get_next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.done() { return None; }
        let token = match self.char().unwrap() {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '.' => Token::Dot,
            ';' => Token::Semi,

            ':' => {
                self.advance();
                match self.char() {
                    Some('=') => Token::Assign,
                    c => panic!("Error: expected '=' after ':' but found {:?}", c),
                }
            }

            '0' ... '9' => self.integer(),

            'a' ... 'z' | 'A' ... 'Z' => self.word(),

             c => panic!("Error: unexpected character: '{}'.", c),
        };
        self.advance();
        Some(token)
    }
}

#[derive(Debug)]
enum AST {
    Compound (Vec<Box<AST>>),
    Assign { lhs: Box<AST>, rhs: Box<AST> },
    Var (String),
    NoOp,
    BinOp { lhs: Box<AST>, op: Token, rhs: Box<AST> },
    UnaryOp { op: Token, rhs: Box<AST> },
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

    fn parse(&mut self) -> AST {
        let ast = self.program();
        match self.current_token {
            None => (),
            _ => panic!("Error: tokens remain after parsing."),
        }
        ast
    }

    /// program : compound_statement Dot
    fn program(&mut self) -> AST {
        let ast = self.compound_statement();
        match self.current_token {
            Some(Token::Dot) => self.advance(),
            ref t => panic!("Error: expected a Dot found a {:?}", t),
        };
        ast
    }

    /// compound_statement : Begin statement_list End
    fn compound_statement(&mut self) -> AST {
        match self.current_token {
            Some(Token::Begin) => self.advance(),
            ref t => panic!("Error: expected a Begin found a {:?}", t),
        };
        let ast = AST::Compound(self.statement_list());
        match self.current_token {
            Some(Token::End) => self.advance(),
            ref t => panic!("Error: expected a End found a {:?}", t),
        };
        ast
    }

    /// statement_list : statement (Semi statement_list)*
    fn statement_list(&mut self) -> Vec<Box<AST>> {
        let mut stmts = vec![Box::new(self.statement())];
        loop {
            match self.current_token {
                Some(Token::Semi) => {
                    self.advance();
                    stmts.push(Box::new(self.statement()))
                }
                _ => break,
            }
        }
        stmts
    }

    /// statement : compound_statement
    ///           | assignment_statement
    ///           | empty
    fn statement(&mut self) -> AST {
        match self.current_token {
            Some(Token::Begin) => self.compound_statement(),
            Some(Token::Id(_)) => self.assignment_statement(),
            _ => self.empty(),
        }
    }

    /// assignment_statement : variable Assign expression
    fn assignment_statement(&mut self) -> AST {
        let lhs = self.variable();
        match self.current_token {
            Some(Token::Assign) => self.advance(),
            ref t => panic!("Error: expected an Assign found a {:?}", t),
        };
        AST::Assign{lhs: Box::new(lhs), rhs: Box::new(self.expression())}
    }

    /// variable : Id
    fn variable(&mut self) -> AST {
        let ast = match self.current_token {
            Some(Token::Id(ref s)) => AST::Var(s.clone()),
            ref t => panic!("Error: expected a Variable found a {:?}", t),
        };
        self.advance();
        ast
    }

    /// empty :
    fn empty(&mut self) -> AST {
        AST::NoOp
    }

    /// factor : (Plus | Minus) factor
    ///        | Integer
    ///        | LParen expression RParen
    ///        | variable
    fn factor(&mut self) -> AST {
        match self.current_token {
            Some(Token::Plus) | Some(Token::Minus) => {
                let token = self.current_token.clone().unwrap();
                self.advance();
                AST::UnaryOp{op: token, rhs: Box::new(self.factor())}
            }

            Some(Token::Integer(i)) => {
                self.advance();
                AST::Num(i)
            }

            Some(Token::LParen) => {
                self.advance();
                let paren_ast = self.expression();
                match self.current_token {
                    Some(Token::RParen) => { self.advance(); paren_ast }
                    ref t => panic!("Error: expected an RParen found a {:?}", t),
                }
            }

            Some(Token::Id(_)) => self.variable(),

            ref t => panic!("Error: expected an integer and found a '{:?}'.", t),
        }
    }

    /// factor ((Star | Slash) factor)*
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

    /// term ((Plus | Minus) term)*
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
    parser: Parser,
    global_scope: HashMap<String, i32>,
}

impl Interpreter {
    fn new(source: String) -> Interpreter {
        Interpreter {
            parser: Parser::new(source),
            global_scope: HashMap::new(),
        }
    }

    fn visit(&mut self, ast: AST) {
        match ast {
            AST::Compound(children) => {
                for child in children {
                    self.visit(*child);
                }
            }
            AST::Assign{lhs, rhs} => match *lhs {
                AST::Var(s) => {
                    let val = self.evaluate(*rhs);
                    self.global_scope.insert(s, val);
                }
                ref t => panic!("Error: expected variable and found {:?}", t),
            },
            AST::NoOp => (),

            ref t => panic!("Error: expected statement and found {:?}", t),
        }
    }

    fn evaluate(&self, ast: AST) -> i32 {
        match ast {
            AST::BinOp{lhs, op, rhs} => match op {
                Token::Plus  => self.evaluate(*lhs) + self.evaluate(*rhs),
                Token::Minus => self.evaluate(*lhs) - self.evaluate(*rhs),
                Token::Star  => self.evaluate(*lhs) * self.evaluate(*rhs),
                Token::Slash => self.evaluate(*lhs) / self.evaluate(*rhs),
                _ => panic!("Error: expected arithmetic operator and found '{:?}'.", op),
            },
            AST::UnaryOp{op, rhs} => match op {
                Token::Plus  => self.evaluate(*rhs),
                Token::Minus => -self.evaluate(*rhs),
                _ => panic!("Error: expected a '+' or '-' and found '{:?}'.", op),
            },
            AST::Var(name) => match self.global_scope.get(&name) {
                Some(val) => *val,
                None => panic!("Error: the variable {} is undefined", name),
            },
            AST::Num(i) => i,
            ref t => panic!("Error: expected an evaluatable expression found {:?}", t),
        }
    }

    fn interpret(&mut self) {
        let ast = self.parser.parse();
        self.visit(ast);
        println!("{:#?}", self.global_scope);
    }
}

fn main() {
    use std::env;

    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => println!("Usage: lox [script]")
    }
}

fn run_file(path: &String) {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(path).expect("Could not open file");
    let mut file_string = String::new();
    file.read_to_string(&mut file_string).expect("Could not read from file");
    run(file_string);
}

macro_rules! prompt(
    ($($arg:tt)*) => { {
        print!($($arg)*);
        stdout().flush().expect("Could not flush stdout");
    } }
);

fn run_prompt() {
    use std::io::{BufRead, stdin, stdout, Write};

    prompt!("calc> ");
    let stdin = stdin();
    for line in stdin.lock().lines() {
        let mut interpreter = Interpreter::new(line.unwrap());
        interpreter.interpret();
        prompt!("calc> ");
    }
}

fn run(source: String) {
    let mut interpreter = Interpreter::new(source);
    interpreter.interpret();
}
