use crate::parser::{Ast, Expr, Literal};
use crate::tokenizer::TokenType;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Rc<Function>),
    NativeFunction(fn(Vec<Value>) -> Value),
    Nil,
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Rc<Expr>,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Function(_) => write!(f, "<fn>"),
            Value::NativeFunction(_) => write!(f, "<native-fn>"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub struct Interpreter {
    ast: Ast,
    // Environments stack: envs[0] is global, last is current scope
    envs: Vec<HashMap<String, Value>>,
    returning: bool,
    ret_val: Value,
}

impl Interpreter {
    pub fn new(ast: Ast) -> Self { 
        let mut interp = Self { ast, envs: vec![HashMap::new()], returning: false, ret_val: Value::Nil };
        // Register built-in native function __rustc_println
        if let Some(global) = interp.envs.last_mut() {
            global.insert("__rustc_println".to_string(), Value::NativeFunction(|args: Vec<Value>| {
                if args.is_empty() {
                    println!("");
                } else {
                    for (i, a) in args.iter().enumerate() {
                        if i > 0 { print!(" "); }
                        print!("{}", a);
                    }
                    println!("");
                }
                Value::Nil
            }));
            // Register built-in native function __rustc_readline
            global.insert("__rustc_readline".to_string(), Value::NativeFunction(|args: Vec<Value>| {
                // Optional prompt (printed without newline)
                if !args.is_empty() {
                    for (i, a) in args.iter().enumerate() {
                        if i > 0 { print!(" "); }
                        print!("{}", a);
                    }
                    use std::io::Write as _;
                    let _ = std::io::stdout().flush();
                }
                let mut line = String::new();
                let _ = std::io::stdin().read_line(&mut line);
                // Trim trailing \r and \n
                while line.ends_with('\n') || line.ends_with('\r') {
                    line.pop();
                }
                Value::String(line)
            }));
        }
        interp
    }

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
                    let _ = self.eval(expr);
                    if self.returning { break; }
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
            Expr::FuncDef { name, params, body } => {
                let func = Rc::new(Function { params: params.clone(), body: body.clone() });
                self.set_var(name, Value::Function(func));
                Value::Nil
            }
            Expr::Call { callee, args } => {
                let cal = self.eval(callee);
                let mut argv = Vec::new();
                for a in args {
                    argv.push(self.eval(a));
                }
                match cal {
                    Value::Function(f) => self.call_function(f, argv),
                    Value::NativeFunction(f) => f(argv),
                    other => panic!("Attempted to call non-function value: {:?}", other),
                }
            }
            Expr::Return(expr) => {
                let v = self.eval(expr);
                self.returning = true;
                self.ret_val = v;
                Value::Nil
            }
        }
    }

    fn as_number(&self, v: Value) -> f64 {
        match v {
            Value::Number(n) => n,
            Value::String(s) => s.parse::<f64>().unwrap_or_else(|_| panic!("Expected number, got string '{}'", s)),
            Value::Boolean(b) => if b { 1.0 } else { 0.0 },
            Value::Function(_) => panic!("Cannot convert function to number"),
            Value::NativeFunction(_) => panic!("Cannot convert native function to number"),
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

    fn call_function(&mut self, func: Rc<Function>, args: Vec<Value>) -> Value {
        if args.len() != func.params.len() {
            panic!("Expected {} arguments, got {}", func.params.len(), args.len());
        }
        // New call scope
        self.push_scope();
        // Bind parameters
        for (i, pname) in func.params.iter().enumerate() {
            if let Some(current) = self.envs.last_mut() {
                current.insert(pname.clone(), args[i].clone());
            }
        }
        // Execute body
        let old_returning = self.returning;
        let old_ret_val = self.ret_val.clone();
        self.returning = false;
        self.ret_val = Value::Nil;
        let _ = self.eval(&func.body);
        // Capture return value if set
        let ret = if self.returning { self.ret_val.clone() } else { Value::Nil };
        // Restore flags
        self.returning = old_returning;
        self.ret_val = old_ret_val;
        // Pop call scope
        self.pop_scope();
        ret
    }
}
