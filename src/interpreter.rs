use crate::parser::{Ast, Expr, Literal};
use crate::tokenizer::TokenType;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub struct Interpreter {
    ast: Ast,
    // Environments stack: envs[0] is global, last is current scope
    envs: Vec<HashMap<String, Value>>,
}

impl Interpreter {
    pub fn new(ast: Ast) -> Self { Self { ast, envs: vec![HashMap::new()] } }

    // Evaluate all top-level expressions, returning their values
    pub fn interpret(&mut self) -> Vec<Value> {
        // Move nodes out to avoid borrowing self while mutably using it in eval
        let nodes = std::mem::take(&mut self.ast.nodes);
        let mut results = Vec::new();
        for expr in &nodes {
            let v = self.eval(expr);
            results.push(v);
        }
        // Put the nodes back so Interpreter can be reused if needed
        self.ast.nodes = nodes;
        results
    }

    // Evaluate a provided AST using the current environment (useful for REPL)
    pub fn interpret_with(&mut self, ast: Ast) -> Vec<Value> {
        let nodes = ast.nodes;
        let mut results = Vec::new();
        for expr in &nodes {
            let v = self.eval(expr);
            results.push(v);
        }
        results
    }

    fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Number(s) => {
                    let n = s.parse::<f64>().unwrap_or(0.0);
                    Value::Number(n)
                }
                Literal::String(s) => Value::String(s.clone()),
            },
            Expr::While { cond, body } => {
                let mut cond_val = self.eval(cond);
                while self.is_truthy(&cond_val) {
                    self.eval(body);
                    cond_val = self.eval(cond);
                }
                Value::Nil
            }
            Expr::If { cond, then, else_ } => {
                let cond = self.eval(cond);
                if self.is_truthy(&cond) {
                    self.eval(then)
                } else {
                    if let Some(else_) = else_ {
                        self.eval(else_)
                    } else {
                        Value::Nil
                    }
                }
            }

            Expr::Block(inner) => {
                // Enter new scope
                self.push_scope();
                for expr in inner.iter() {
                    self.eval(expr);
                }
                // Exit scope
                self.pop_scope();
                Value::Nil
            }

            Expr::VarGet(name) => {
                match self.get_var(name) {
                    Some(v) => v,
                    None => panic!("Undefined variable '{}'.", name),
                }
            }

            Expr::Grouping(inner) => self.eval(inner),
            Expr::Unary { op, right } => {
                let rv = self.eval(right);
                match op {
                    TokenType::Minus => Value::Number(-self.as_number(rv)),
                    TokenType::Bang => Value::Boolean(!self.is_truthy(&rv)),
                    _ => panic!("Unsupported unary operator: {:?}", op),
                }
            }
            Expr::Assign { name, value } => {
                let v = self.eval(value);
                self.set_var(name, v);
                Value::Nil
            }
            Expr::Print(expr) => {
                let rv = self.eval(expr);
                println!("{}", rv);
                Value::Nil
            }
            Expr::Binary { left, op, right } => {
                let lv = self.eval(left);
                let rv = self.eval(right);
                match op {
                    TokenType::Plus => match (lv, rv) {
                        (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
                        (Value::String(a), Value::String(b)) => Value::String(a + &b),
                        (a, b) => panic!("Type error: cannot add {:?} and {:?}", a, b),
                    },
                    TokenType::Minus => Value::Number(self.as_number(lv) - self.as_number(rv)),
                    TokenType::Star => Value::Number(self.as_number(lv) * self.as_number(rv)),
                    TokenType::Slash => {
                        let denom = self.as_number(rv);
                        if denom == 0.0 { panic!("Division by zero"); }
                        Value::Number(self.as_number(lv) / denom)
                    }
                    TokenType::Greater => Value::Boolean(self.as_number(lv) > self.as_number(rv)),
                    TokenType::GreaterEqual => Value::Boolean(self.as_number(lv) >= self.as_number(rv)),
                    TokenType::Less => Value::Boolean(self.as_number(lv) < self.as_number(rv)),
                    TokenType::LessEqual => Value::Boolean(self.as_number(lv) <= self.as_number(rv)),
                    TokenType::EqualEqual => Value::Boolean(self.eq_value(&lv, &rv)),
                    TokenType::BangEqual => Value::Boolean(!self.eq_value(&lv, &rv)),
                    other => panic!("Unsupported binary operator: {:?}", other),
                }
            }
        }
    }

    fn as_number(&self, v: Value) -> f64 {
        match v {
            Value::Number(n) => n,
            Value::String(s) => s.parse::<f64>().unwrap_or_else(|_| panic!("Expected number, got string '{}'", s)),
            Value::Boolean(b) => if b { 1.0 } else { 0.0 },
            Value::Nil => 0.0,
        }
    }

    fn is_truthy(&self, v: &Value) -> bool {
        match v {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    fn eq_value(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Number(x), Value::Number(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Boolean(x), Value::Boolean(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    // Scope management helpers
    fn push_scope(&mut self) {
        self.envs.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        // Never pop the last (global) scope
        if self.envs.len() > 1 {
            self.envs.pop();
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        for scope in self.envs.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn set_var(&mut self, name: &str, value: Value) {
        // Assign into the nearest existing scope; if not found, define in current scope
        for scope in self.envs.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value.clone());
                return;
            }
        }
        if let Some(current) = self.envs.last_mut() {
            current.insert(name.to_string(), value);
        }
    }
}
