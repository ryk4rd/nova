use crate::tokenizer::{Token, TokenType};

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    VarGet(String),
    Grouping(Box<Expr>),
    Unary { op: TokenType, right: Box<Expr> },
    Binary { left: Box<Expr>, op: TokenType, right: Box<Expr> },
    Print(Box<Expr>),
    Assign { name: String, value: Box<Expr> },
    If { cond: Box<Expr>, then: Box<Expr>, else_: Option<Box<Expr>> },
    Block(Box<Vec<Expr>>),
    While { cond: Box<Expr>, body: Box<Expr> },
}


#[derive(Debug)]
pub enum Literal {
    Number(String),
    String(String),
}

#[derive(Debug)]
pub struct Ast {
    pub nodes: Vec<Expr>,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(mut self) -> Ast {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            // Parse an expression; if we fail to advance, break to avoid infinite loop
            let start = self.current;
            let expr = self.program();
            nodes.push(expr);
            if self.current == start {
                break;
            }
            // Optional: consume a semicolon between expressions if present
            self.match_token(&[TokenType::Semicolon]);
        }
        Ast { nodes }
    }

    fn program(&mut self) -> Expr {
        self.declaration()
    }

    fn declaration(&mut self) -> Expr {
        self.statement()
    }


    fn statement(&mut self) -> Expr {
        if self.match_token(&[TokenType::Print]) {
            return self.print_statement();
        }

        if self.match_token(&[TokenType::If]) {
            return self.if_statement();
        }

        if self.match_token(&[TokenType::While]) {
            return self.while_statement();
        }

        self.expr_stmt()
    }

    fn while_statement(&mut self) -> Expr {
        let cond = self.expression();
        let body = self.statement();
        Expr::While { cond: Box::new(cond), body: Box::new(body) }
    }

    fn print_statement(&mut self) -> Expr {
        let expr = self.expression();
        Expr::Print(Box::new(expr))
    }


    fn expr_stmt(&mut self) -> Expr {
        self.expression()
    }

    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    fn assignment(&mut self) -> Expr {
        let expr = self.equality();


        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment();
            if let Expr::VarGet(name) = expr {
                return Expr::Assign {name, value: Box::new(value) };
            }
            panic!("Invalid assignment target: {}", equals.value().unwrap_or(""));
        }
        expr
    }


    fn if_statement(&mut self) -> Expr {
        let cond = self.expression();
        let then = self.statement();

        let else_ = if self.match_token(&[TokenType::Else]) {
            Some(Box::new(self.statement()))
        } else {
            None
        };

        //add else statemen
        //let else_ = self.statement();
        Expr::If { cond: Box::new(cond), then: Box::new(then), else_: else_ }
    }


    // == | !=
    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while self.match_token(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let op = self.previous().token_type();
            let right = self.comparison();
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.addition();
        while self.match_token(&[TokenType::Greater, TokenType::GreaterEqual,
            TokenType::Less, TokenType::LessEqual]) {
            let op = self.previous().token_type();
            let right = self.addition();
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        expr
    }

    // + | -
    fn addition(&mut self) -> Expr {
        let mut expr = self.term();
        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let op = self.previous().token_type();
            let right = self.term();
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        expr
    }

    // * | /
    fn term(&mut self) -> Expr {
        let mut expr = self.unary();
        while self.match_token(&[TokenType::Star, TokenType::Slash]) {
            let op = self.previous().token_type();
            let right = self.unary();
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        expr
    }

    // ! | -
    fn unary(&mut self) -> Expr {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().token_type();
            let right = self.unary();
            return Expr::Unary { op, right: Box::new(right) };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if self.match_token(&[TokenType::Number]) {
            let lex = self.previous().value().unwrap_or("").to_string();
            return Expr::Literal(Literal::Number(lex));
        }
        if self.match_token(&[TokenType::String]) {
            let lex = self.previous().value().unwrap_or("").to_string();
            return Expr::Literal(Literal::String(lex));
        }
        if self.match_token(&[TokenType::Identifier]) {
            let name = self.previous().value().unwrap_or("").to_string();
            return Expr::VarGet(name);
        }
        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(TokenType::RightParen, ")");
            return Expr::Grouping(Box::new(expr));
        }

        if self.match_token(&[TokenType::LeftCurly]) {
        
            let mut exprs = vec![];

            while !self.check(TokenType::RightCurly) {
                exprs.push(self.declaration());
            }
            self.consume(TokenType::RightCurly, "}");
            return Expr::Block(Box::new(exprs));
        }
        // Fallback: if nothing matches, return a dummy literal from current token value
        // or panic for now because grammar expects a primary

        dbg!(self.peek());
        panic!("Expected expression at token index {}", self.current);
    }

    // utilities
    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for &t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, t: TokenType, _context: &str) {
        if self.check(t) { self.advance(); return; }
        panic!("Expected token {:?} before {} at index {}", t, _context, self.current);
    }

    fn check(&self, t: TokenType) -> bool {
        if self.is_at_end() { return false; }
        self.peek().token_type() == t
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() { self.current += 1; }
        self.previous_ref()
    }

    fn is_at_end(&self) -> bool { self.current >= self.tokens.len() }

    fn peek(&self) -> &Token { &self.tokens[self.current] }

    fn previous(&self) -> Token { self.tokens[self.current - 1].clone() }
    fn previous_ref(&self) -> &Token { &self.tokens[self.current - 1] }
}
