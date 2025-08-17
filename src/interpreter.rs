use crate::parser::{Ast, Expr, Literal, Parser};
use crate::tokenizer::{self, TokenType};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;
use std::fs;
use std::env;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Function(Rc<Function>),
    NativeFunction(Rc<NativeFn>),
    Nil,
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub vararg: Option<String>,
    pub body: Rc<Expr>,
}

// Runtime error type and native function signature
#[derive(Clone)]
pub struct RuntimeError { pub message: String }
impl RuntimeError { pub fn new(msg: impl Into<String>) -> Self { Self { message: msg.into() } } }
impl std::fmt::Display for RuntimeError { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.message) } }
impl std::fmt::Debug for RuntimeError { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "RuntimeError({})", self.message) } }

pub type NativeFn = dyn Fn(&[Value]) -> Result<Value, RuntimeError>;

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
            Value::List(items) => {
                write!(f, "[")?;
                for (i, it) in items.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", it)?;
                }
                write!(f, "]")
            }
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
    current_dir: PathBuf,
    included: HashSet<PathBuf>,
}

impl Interpreter {
    pub fn new(ast: Ast) -> Self { 
        let mut interp = Self { 
            ast, 
            envs: vec![HashMap::new()], 
            returning: false, 
            ret_val: Value::Nil,
            current_dir: env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            included: HashSet::new(),
        };
        interp.register_builtins();
        interp
    }

    pub fn set_current_dir<P: Into<PathBuf>>(&mut self, dir: P) {
        self.current_dir = dir.into();
    }

    // Evaluate all top-level expressions, returning their values
    pub fn interpret(&mut self) -> Vec<Value> {
        // Move nodes out to avoid borrowing self while mutably using it in eval
        let nodes = std::mem::take(&mut self.ast.nodes);
        // Ensure prelude is included once per top-level program
        let prelude = Expr::Include("prelude.nova".to_string());
        let _ = self.eval(&prelude);
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
                        (a, b) => panic!("Type error: cannot add {} and {}", a, b),
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
            Expr::FuncDef { name, params, vararg, body } => {
                let func = Rc::new(Function { params: params.clone(), vararg: vararg.clone(), body: body.clone() });
                self.set_var(name, Value::Function(func));
                Value::Nil
            }
            Expr::Call { callee, args } => {
                // If calling an identifier that is not defined, report "function" not "variable"
                if let Expr::VarGet(name) = &**callee {
                    if self.get_var(name).is_none() {
                        panic!("Function not found '{}'.", name);
                    }
                }
                let cal = self.eval(callee);
                let mut argv = Vec::new();
                for a in args {
                    argv.push(self.eval(a));
                }
                match cal {
                    Value::Function(f) => self.call_function(f, argv),
                    Value::NativeFunction(f) => (f)(&argv).unwrap_or_else(|e| panic!("Native function error: {}", e)),
                    other => panic!("Attempted to call non-function value: {}", other),
                }
            }
            Expr::Include(path) => {
                // Resolve include path relative to current_dir if not absolute
                let mut target = PathBuf::from(path.clone());
                if !target.is_absolute() {
                    target = self.current_dir.join(&target);
                }
                let canon = fs::canonicalize(&target).unwrap_or_else(|_| target.clone());
                if self.included.contains(&canon) {
                    return Value::Nil;
                }
                // Read file
                let contents = fs::read_to_string(&target)
                    .unwrap_or_else(|e| panic!("Include error: cannot read '{}': {}", target.display(), e));
                // Mark as included to prevent cycles
                self.included.insert(canon.clone());
                // Tokenize and parse
                let tokens = tokenizer::scan(contents);
                let ast = Parser::new(tokens).parse();
                // Temporarily switch current_dir to included file's directory
                let prev_dir = self.current_dir.clone();
                let new_dir = canon.parent().unwrap_or(&prev_dir).to_path_buf();
                self.current_dir = new_dir;
                let _ = self.interpret_with(ast);
                self.current_dir = prev_dir;
                Value::Nil
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
            Value::List(_) => panic!("Cannot convert list to number"),
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
                // Prevent overwriting existing functions (user-defined or native)
                if let Some(existing) = scope.get(name) {
                    match existing {
                        Value::Function(_) | Value::NativeFunction(_) => {
                            panic!("Cannot overwrite function '{}'.", name);
                        }
                        _ => {}
                    }
                }
                scope.insert(name.to_string(), value.clone());
                return;
            }
        }
        if let Some(current) = self.envs.last_mut() {
            current.insert(name.to_string(), value);
        }
    }

    fn call_function(&mut self, func: Rc<Function>, args: Vec<Value>) -> Value {
        let fixed = func.params.len();
        match &func.vararg {
            None => {
                if args.len() != fixed {
                    panic!("Expected {} arguments, got {}", fixed, args.len());
                }
            }
            Some(_) => {
                if args.len() < fixed {
                    panic!("Expected at least {} arguments, got {}", fixed, args.len());
                }
            }
        }
        // New call scope
        self.push_scope();
        // Bind fixed parameters
        for (i, pname) in func.params.iter().enumerate() {
            if let Some(current) = self.envs.last_mut() {
                current.insert(pname.clone(), args[i].clone());
            }
        }
        // Bind vararg as list if present
        if let Some(varname) = &func.vararg {
            let rest = if args.len() > fixed { args[fixed..].to_vec() } else { Vec::new() };
            if let Some(current) = self.envs.last_mut() {
                current.insert(varname.clone(), Value::List(rest));
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


impl Interpreter {
    fn add_native<F>(&mut self, name: &str, min_args: usize, max_args: Option<usize>, f: F)
    where
        F: Fn(&[Value]) -> Result<Value, RuntimeError> + 'static,
    {
        let name_owned = name.to_string();
        let name_for_key = name_owned.clone();
        let func: Rc<NativeFn> = Rc::new(move |args: &[Value]| -> Result<Value, RuntimeError> {
            if args.len() < min_args {
                return Err(RuntimeError::new(format!(
                    "{} expects at least {} args, got {}",
                    name_owned, min_args, args.len()
                )));
            }
            if let Some(max) = max_args {
                if args.len() > max {
                    return Err(RuntimeError::new(format!(
                        "{} expects at most {} args, got {}",
                        name_owned, max, args.len()
                    )));
                }
            }
            f(args)
        });
        if let Some(global) = self.envs.last_mut() {
            global.insert(name_for_key, Value::NativeFunction(func));
        }
    }

    fn register_builtins(&mut self) {
        // __rustc_println: prints all args space-separated and newline
        self.add_native("__rustc_println", 0, None, |args: &[Value]| {
            // Expand a single list argument
            let mut iter: Vec<&Value> = Vec::new();
            if args.len() == 1 {
                if let Value::List(items) = &args[0] {
                    for it in items { iter.push(it); }
                } else {
                    iter.push(&args[0]);
                }
            } else {
                for a in args { iter.push(a); }
            }
            if iter.is_empty() {
                println!("");
            } else {
                for (i, a) in iter.iter().enumerate() {
                    if i > 0 { print!(" "); }
                    print!("{}", a);
                }
                println!("");
            }
            Ok(Value::Nil)
        });

        self.add_native("int", 1, Some(1), |args: &[Value]| {

            for a in args {
                if let Value::Number(n) = a {
                    return Ok(Value::Number(n.floor()));
                }
            }
            Err(RuntimeError::new("int expects a number".to_string()))
        });

        // __rustc_readline: optional prompt (args joined by space), returns String or Nil on EOF
        self.add_native("__rustc_readline", 0, None, |args: &[Value]| {
            if !args.is_empty() {
                for (i, a) in args.iter().enumerate() {
                    if i > 0 { print!(" "); }
                    print!("{}", a);
                }
                use std::io::Write as _;
                let _ = std::io::stdout().flush();
            }
            use std::io;
            let mut line = String::new();
            let stdin = io::stdin();
            match stdin.read_line(&mut line) {
                Ok(n) => {
                    if n == 0 { return Ok(Value::Nil); }
                }
                Err(e) => {
                    return Err(RuntimeError::new(format!("readline failed: {}", e)));
                }
            }
            while line.ends_with('\n') || line.ends_with('\r') {
                line.pop();
            }
            Ok(Value::String(line))
        });
    }
}
