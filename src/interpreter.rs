use crate::parser::{Ast, Expr, Literal};
use crate::tokenizer::TokenType;

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
}

impl Interpreter {
    pub fn new(ast: Ast) -> Self { Self { ast } }

    // Evaluate all top-level expressions, returning their values
    pub fn interpret(&self) -> Vec<Value> {
        self.ast
            .nodes
            .iter()
            .map(|expr| self.eval(expr))
            .collect()
    }

    fn eval(&self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Number(s) => {
                    let n = s.parse::<f64>().unwrap_or(0.0);
                    Value::Number(n)
                }
                Literal::String(s) => Value::String(s.clone()),
            },
            Expr::Variable(name) => {
                panic!("Variable '{}' not implemented (no environment)", name);
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
}