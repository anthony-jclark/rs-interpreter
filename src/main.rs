///
/// program : PROGRAM variable SEMI block DOT
/// block : declarations compound_statement
/// declarations : VAR (variable_declarations SEMI)+
///              | empty
/// variable_declarations : ID (COMMA ID)* COLON type_specifier
/// type_specifier : INTEGER | REAL
/// compound_statement : BEGIN statements END
/// statements : statement (SEMI statements)*
/// statement : compound_statement
///           | assignment_statement
///           | empty
/// assignment_statement : variable ASSIGN expression
/// variable : ID
/// empty :
///
/// expression : term ((PLUS | MINUS) term)*
/// term : factor ((STAR | DIV | SLASH) factor)*
/// factor : (PLUS | MINUS) factor
///        | INTEGERLITERAL
///        | REALLITERAL
///        | LPAREN expression RPAREN
///        | variable
///

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Token {
    // Arithmetic
    Plus, Minus, Star, Slash, Div,
    // Grouping
    LParen, RParen,
    // Keywords
    Begin, End, Program, Var, Integer, Real,
    // Punctuation
    Dot, Semi, Assign, Colon, Comma,
    // Literals
    IntegerLiteral(i32), RealLiteral(f64),
    // Identifiers
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

    fn skip_comment(&mut self) {
        while !self.done() && self.source[self.position] != '}' {
            self.advance();
        }
        self.advance();
    }

    fn number(&mut self) -> Token {
        let start = self.position;
        while self.next_char_is(|c| c.is_digit(10)) {
            self.advance();
        }
        if self.next_char_is(|c| c == '.') {
            self.advance();
            while self.next_char_is(|c| c.is_digit(10)) {
                self.advance();
            }
            Token::RealLiteral(
                self.string(start, self.position + 1).parse::<f64>().unwrap())
        } else {
            Token::IntegerLiteral(
                self.string(start, self.position + 1).parse::<i32>().unwrap())
        }

    }

    fn word(&mut self) -> Token {
        use std::ascii::AsciiExt;

        let start = self.position;
        while self.next_char_is(|c| c.is_alphanumeric() && c.is_ascii()) {
            self.advance();
        }

        let word = self.string(start, self.position + 1).to_lowercase();
        match word.as_ref() {
            "begin"   => Token::Begin,
            "div"     => Token::Div,
            "end"     => Token::End,
            "integer" => Token::Integer,
            "program" => Token::Program,
            "real"    => Token::Real,
            "var"     => Token::Var,
            _         => Token::Id(word),
        }
    }

    fn get_next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.done() { return None; }

        // Skip comments
        while let Some('{') = self.char() {
            self.skip_comment();
            self.skip_whitespace();
        }

        let token = match self.char().unwrap() {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '.' => Token::Dot,
            ';' => Token::Semi,
            ',' => Token::Comma,

            ':' if self.next_char_is(|c| c == '=') => { self.advance(); Token::Assign }
            ':' => Token::Colon,

            '0'...'9' => self.number(),

            'a'...'z' | 'A'...'Z' | '_' => self.word(),

             c => panic!("Error: unexpected character: '{}'.", c),
        };
        self.advance();
        Some(token)
    }
}


/// Program : PROGRAM variable SEMI block DOT
/// Block : declarations compound_statement
/// Declarations : VAR (variable_declarations SEMI)+
///              | empty
/// Variable_declarations : ID (COMMA ID)* COLON type_specifier
/// Type_specifier : INTEGER | REAL
/// Compound_statement : BEGIN statements END
/// Statements : statement (SEMI statements)*
/// Statement : compound_statement
///           | assignment_statement
///           | empty
/// Assignment_statement : variable ASSIGN expression
/// Variable : ID
/// Empty :
///
/// Expression : term ((PLUS | MINUS) term)*
/// Term : factor ((STAR | DIV | SLASH) factor)*
/// Factor : (PLUS | MINUS) factor
///        | INTEGERLITERAL
///        | REALLITERAL
///        | LPAREN expression RPAREN
///        | variable

#[derive(Debug)]
enum AST {
    Program { variable: Box<AST>, block: Box<AST> },
    Block { declarations: Box<AST>, compound_statement: Box<AST> },
    Declarations (Vec<Box<AST>>);
    VariableDeclarations { variables: Vec<Box<AST>>, type_specifier: Box<AST> },
    TypeSpecifier (Token),
    CompoundStatement (Box<AST>),
    Statements (Vec<Box<AST>>)
    Assign { lhs: Box<AST>, rhs: Box<AST> },
    Variable (String),
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

    /// program : PROGRAM variable SEMI block DOT
    fn program(&mut self) -> AST {
        let ast = self.compound_statement();
        match self.current_token {
            Some(Token::Dot) => self.advance(),
            ref t => panic!("Error: expected a Dot found a {:?}", t),
        };
        ast
    }

    /// block : declarations compound_statement
    fn block(&mut self) -> AST {
        AST::Block {
            declarations: self.declarations(),
            compound_statement: Box::new(self.compound_statement()),
        }
    }

    /// declarations : VAR (variable_declarations SEMI)+
    ///              | empty
    fn declarations(&mut self) -> Vec<Box<AST>> {
        let mut declarations: Vec<Box<AST>> = Vec::new();
        match self.current_token {
            Some(Token::Var) => {
                self.advance();
                while let Some(Token::Id(_)) = self.current_token {
                    declarations.push(Box::new(self.variable_declaration()));
                    match self.current_token {
                        Some(Token::Semi) => self.advance(),
                        ref t => panic!("Error: expected a Semi found a {:?}", t),
                    }
                }
            }
            _ => (),
        };
        declarations
    }

    /// variable_declarations : ID (COMMA ID)* COLON type_specifier
    fn variable_declarations(&mut self) -> AST {
        unimplemented!();
    }

    /// type_specifier : INTEGER | REAL
    fn type_specifier(&mut self) -> AST {
        unimplemented!();
    }

    /// compound_statement : BEGIN statements END
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

    /// statements : statement (SEMI statements)*
    fn statements(&mut self) -> Vec<Box<AST>> {
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

    /// assignment_statement : variable ASSIGN expression
    fn assignment_statement(&mut self) -> AST {
        let lhs = self.variable();
        match self.current_token {
            Some(Token::Assign) => self.advance(),
            ref t => panic!("Error: expected an Assign found a {:?}", t),
        };
        AST::Assign{lhs: Box::new(lhs), rhs: Box::new(self.expression())}
    }

    /// variable : ID
    fn variable(&mut self) -> AST {
        let ast = match self.current_token {
            Some(Token::Id(ref s)) => AST::Variable(s.clone()),
            ref t => panic!("Error: expected a Variable found a {:?}", t),
        };
        self.advance();
        ast
    }

    /// empty :
    fn empty(&mut self) -> AST {
        AST::NoOp
    }

    /// expression : term ((PLUS | MINUS) term)*
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

    /// term : factor ((STAR | DIV | SLASH) factor)*
    fn term(&mut self) -> AST {
        let mut ast = self.factor();

        loop {
            match self.current_token {
                Some(Token::Star)  => (),
                Some(Token::Div) => (),
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

    /// factor : (PLUS | MINUS) factor
    ///        | INTEGERLITERAL
    ///        | REALLITERAL
    ///        | LPAREN expression RPAREN
    ///        | variable
    fn factor(&mut self) -> AST {
        match self.current_token {
            Some(Token::Plus) | Some(Token::Minus) => {
                let token = self.current_token.clone().unwrap();
                self.advance();
                AST::UnaryOp{op: token, rhs: Box::new(self.factor())}
            }

            Some(Token::IntegerLiteral(i)) => {
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
                AST::Variable(s) => {
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
                Token::Div   => self.evaluate(*lhs) / self.evaluate(*rhs),
                _ => panic!("Error: expected arithmetic operator and found '{:?}'.", op),
            },
            AST::UnaryOp{op, rhs} => match op {
                Token::Plus  => self.evaluate(*rhs),
                Token::Minus => -self.evaluate(*rhs),
                _ => panic!("Error: expected a '+' or '-' and found '{:?}'.", op),
            },
            AST::Variable(name) => match self.global_scope.get(&name) {
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
