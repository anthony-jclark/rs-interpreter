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

        // Skip comments
        while let Some('{') = self.char() {
            self.skip_comment();
            self.skip_whitespace();
        }

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

#[derive(Debug, Clone)]
enum AST {
    Program { name: Box<AST>, block: Box<AST> },
    Block { declarations: Vec<Box<AST>>, compound_statement: Box<AST> },
    VariableDeclaration { name: String, type_specifier: Box<AST> },
    TypeSpecifier (Token),
    CompoundStatement (Vec<Box<AST>>),
    AssignmentStatement { lhs: Box<AST>, rhs: Box<AST> },
    Variable (String),
    Empty,
    BinaryOperator { lhs: Box<AST>, op: Token, rhs: Box<AST> },
    UnaryOperator { op: Token, rhs: Box<AST> },
    Integer (i32),
    Real (f64),
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
        match self.current_token {
            Some(Token::Program) => self.advance(),
            ref t => panic!("Error: expected a Program found a {:?}", t),
        };
        let name = self.variable();
        match self.current_token {
            Some(Token::Semi) => self.advance(),
            ref t => panic!("Error: expected a Semi found a {:?}", t),
        };
        let block = self.block();
        match self.current_token {
            Some(Token::Dot) => self.advance(),
            ref t => panic!("Error: expected a Dot found a {:?}", t),
        };
        AST::Program{name: Box::new(name), block: Box::new(block)}
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
                    declarations.extend(self.variable_declarations());
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
    fn variable_declarations(&mut self) -> Vec<Box<AST>> {
        let mut ids: Vec<String> = Vec::new();
        match self.current_token {
            Some(Token::Id(ref s)) => ids.push(s.clone()),
            ref t => panic!("Error: expected an Id found a {:?}", t),
        }
        self.advance();
        while let Some(Token::Comma) = self.current_token {
            self.advance();
            match self.current_token {
                Some(Token::Id(ref s)) => ids.push(s.clone()),
                ref t => panic!("Error: expected an Id found a {:?}", t),
            }
            self.advance();
        }
        match self.current_token {
            Some(Token::Colon) => self.advance(),
            ref t => panic!("Error: expected a Colon found a {:?}", t),
        }
        let type_specifier = self.type_specifier();
        ids.into_iter().map(|s| Box::new(AST::VariableDeclaration{
            name: s,
            type_specifier: Box::new(type_specifier.clone()),
        })).collect()
    }

    /// type_specifier : INTEGER | REAL
    fn type_specifier(&mut self) -> AST {
        match self.current_token {
            // Some(Token::IntegerLiteral(int)) => { self.advance(); AST::Integer(int) }
            // Some(Token::RealLiteral(real)) => { self.advance(); AST::Real(rea
            Some(Token::Integer) => { self.advance(); AST::TypeSpecifier(Token::Integer) }
            Some(Token::Real)    => { self.advance(); AST::TypeSpecifier(Token::Real) }
            ref t => panic!("Error: expected a type_specifier found a {:?}", t),
        }
    }

    /// compound_statement : BEGIN statements END
    fn compound_statement(&mut self) -> AST {
        match self.current_token {
            Some(Token::Begin) => self.advance(),
            ref t => panic!("Error: expected a Begin found a {:?}", t),
        };
        let ast = AST::CompoundStatement(self.statements());
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
        AST::AssignmentStatement{lhs: Box::new(lhs), rhs: Box::new(self.expression())}
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
        AST::Empty
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
            ast = AST::BinaryOperator {
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
                Some(Token::Div)   => (),
                Some(Token::Slash) => (),
                _ => break,
            }
            let token = self.current_token.clone().unwrap();
            self.advance();
            ast = AST::BinaryOperator {
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
                AST::UnaryOperator{op: token, rhs: Box::new(self.factor())}
            }

            Some(Token::IntegerLiteral(i)) => {
                self.advance();
                AST::Integer(i)
            }

            Some(Token::RealLiteral(r)) => {
                self.advance();
                AST::Real(r)
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

    // Program { name: Box<AST>, block: Box<AST> },
    // Block { declarations: Vec<Box<AST>>, compound_statement: Box<AST> },
    // VariableDeclaration { name: String, type_specifier: Box<AST> },
    // TypeSpecifier (Token),
    // CompoundStatement (Vec<Box<AST>>),
    // AssignmentStatement { lhs: Box<AST>, rhs: Box<AST> },
    // Empty,
    fn visit(&mut self, ast: AST) {
        match ast {
            AST::Program{name, block} => self.visit(*block),
            AST::Block{declarations, compound_statement} => {
                for vardec in declarations {
                    self.visit(*vardec)
                }
                self.visit(*compound_statement);
            }
            AST::VariableDeclaration{name, type_specifier} => (),
            AST::CompoundStatement(children) => {
                for child in children {
                    self.visit(*child);
                }
            }
            AST::AssignmentStatement{lhs, rhs} => match *lhs {
                AST::Variable(s) => {
                    let val = self.evaluate(*rhs);
                    self.global_scope.insert(s, val);
                }
                ref t => panic!("Error: expected variable and found {:?}", t),
            },
            AST::Empty => (),

            ref t => panic!("Error: expected statement and found {:?}", t),
        }
    }

    // BinaryOperator { lhs: Box<AST>, op: Token, rhs: Box<AST> },
    // UnaryOperator { op: Token, rhs: Box<AST> },
    // Variable (String),
    // Integer (i32),
    // Real (f64),
    fn evaluate(&self, ast: AST) -> i32 {
        match ast {
            AST::BinaryOperator{lhs, op, rhs} => match op {
                Token::Plus  => self.evaluate(*lhs) + self.evaluate(*rhs),
                Token::Minus => self.evaluate(*lhs) - self.evaluate(*rhs),
                Token::Star  => self.evaluate(*lhs) * self.evaluate(*rhs),
                Token::Div   => self.evaluate(*lhs) / self.evaluate(*rhs),
                Token::Slash => self.evaluate(*lhs) / self.evaluate(*rhs),
                _ => panic!("Error: expected arithmetic operator and found '{:?}'.", op),
            },
            AST::UnaryOperator{op, rhs} => match op {
                Token::Plus  => self.evaluate(*rhs),
                Token::Minus => -self.evaluate(*rhs),
                _ => panic!("Error: expected a '+' or '-' and found '{:?}'.", op),
            },
            AST::Variable(name) => match self.global_scope.get(&name) {
                Some(val) => *val,
                None => panic!("Error: the variable {} is undefined", name),
            },
            AST::Integer(i) => i,
            AST::Real(r) => r as i32,
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
        _ => println!("Usage: rs_interpreter [script]")
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
