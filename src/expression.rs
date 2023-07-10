use std::any::Any;
use std::ops::Deref;

use crate::environment::Environment;
use crate::error::Error;

#[derive(Clone)]
pub(crate) enum ByteCode {
    LoadConst(f64),
    LoadItem{ index: usize },
    Call{ func_index: usize, num_args: usize, line: usize, column: usize },
    LessThan,
    LessEqual,
    GreaterEqual,
    GreaterThan,
    Equal,
    Add,
    Sub,
    Multiply,
    Divide,
    Remainder,
    JumpIfZero{ offset: usize },
    Jump{ offset: usize},
}

impl std::fmt::Display for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteCode::LoadConst(val) => write!(f, "load {}", val),
            ByteCode::LoadItem { index } => write!(f, "load [{}]", index),
            ByteCode::Call { func_index, num_args, line, column } =>
                write!(f, "call, {}, {} @ {}:{}", func_index, num_args, line, column),
            ByteCode::LessThan => write!(f, "lessThan"),
            ByteCode::LessEqual => write!(f, "lessEqual"),
            ByteCode::GreaterEqual => write!(f, "greaterEqual"),
            ByteCode::GreaterThan => write!(f, "greaterThan"),
            ByteCode::Equal => write!(f, "eq"),
            ByteCode::Add => write!(f, "add"),
            ByteCode::Sub => write!(f, "sub"),
            ByteCode::Multiply => write!(f, "mult"),
            ByteCode::Divide => write!(f, "div"),
            ByteCode::Remainder => write!(f, "remainder"),
            ByteCode::JumpIfZero { offset } => write!(f, "jz {}", offset),
            ByteCode::Jump { offset } => write!(f, "jmp {}", offset),
        }
    }
}

pub(crate) trait Expr: core::fmt::Debug {
    fn emit_bytecode(&self, env: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error>;

    fn fold(&mut self, env: &Environment) -> Option<Box<dyn Expr>>;

    fn as_any(&self) -> &dyn Any;
    /// Does not compare buffer positions
    fn values_equal(&self, other: &dyn Expr) -> bool;
}

pub(crate) type BoxedExpr = Box<dyn Expr>;

#[derive(Debug)]
pub(crate) struct ConditionalExpr {
    pub condition: Box<dyn Expr>,
    pub when_true: Box<dyn Expr>,
    pub when_false: Box<dyn Expr>,
}

impl Expr for ConditionalExpr {
    fn emit_bytecode(&self, env: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error> {
        // Bytecode overview:
        //   jz false
        //   <true logic>
        //   jmp done
        // false:
        //   <false logic>
        // done:
        //   ; continue program flow

        let mut false_path = Vec::new();
        self.when_false.emit_bytecode(env, &mut false_path)?;

        let mut true_path = Vec::new();
        self.when_true.emit_bytecode(env, &mut true_path)?;
        true_path.push(ByteCode::Jump{ offset: false_path.len() });

        self.condition.emit_bytecode(env, bc)?;
        bc.push(ByteCode::JumpIfZero{ offset: true_path.len() });
        bc.extend_from_slice(&true_path);
        bc.extend_from_slice(&false_path);
        Ok(())
    }

    fn fold(&mut self, env: &Environment) -> Option<Box<dyn Expr>> {
        if let Some(cond) = self.condition.fold(env) {
            self.condition = cond;
        }

        match self.condition.as_any().downcast_ref::<f64>()? {
            val if val.is_nan() => Some(Box::new(f64::NAN)),
            val if *val == 0.0 => self.when_false.fold(env),
            _ => self.when_true.fold(env),
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn values_equal(&self, other: &dyn Expr) -> bool {
        other.as_any()
            .downcast_ref::<ConditionalExpr>()
            .map_or(false, |expr|
                self.condition.values_equal(expr.condition.deref()) &&
                self.when_true.values_equal(expr.when_true.deref()) &&
                self.when_false.values_equal(expr.when_false.deref())
            )
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum BinaryOperator {
    LessThan,
    LessEqual,
    GreaterEqual,
    GreaterThan,
    Equal,
    Add,
    Sub,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug)]
pub(crate) struct BinaryExpr {
    pub left: Box<dyn Expr>,
    pub operator: BinaryOperator,
    pub right: Box<dyn Expr>,
}

impl Expr for BinaryExpr {
    fn emit_bytecode(&self, env: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error> {
        self.right.emit_bytecode(env, bc)?;
        self.left.emit_bytecode(env, bc)?;
        bc.push(match self.operator {
            BinaryOperator::LessThan => ByteCode::LessThan,
            BinaryOperator::LessEqual => ByteCode::LessEqual,
            BinaryOperator::GreaterEqual => ByteCode::GreaterEqual,
            BinaryOperator::GreaterThan => ByteCode::GreaterThan,
            BinaryOperator::Equal => ByteCode::Equal,
            BinaryOperator::Add => ByteCode::Add,
            BinaryOperator::Sub => ByteCode::Sub,
            BinaryOperator::Multiply => ByteCode::Multiply,
            BinaryOperator::Divide => ByteCode::Divide,
            BinaryOperator::Remainder => ByteCode::Remainder,
        });
        Ok(())
    }

    fn fold(&mut self, env: &Environment) -> Option<Box<dyn Expr>> {
        if let Some(left) = self.left.fold(env) {
            self.left = left;
        }

        if let Some(right) = self.right.fold(env) {
            self.right = right;
        }

        let a = self.left.as_any().downcast_ref::<f64>()?;
        let b = self.right.as_any().downcast_ref::<f64>()?;
        if a.is_nan() || b.is_nan() {
            return Some(Box::new(f64::NAN))
        }

        let result = match self.operator {
            BinaryOperator::LessThan => (a < b) as i32 as f64, 
            BinaryOperator::LessEqual => (a <= b) as i32 as f64,
            BinaryOperator::GreaterEqual => (a >= b) as i32 as f64,
            BinaryOperator::GreaterThan => (a > b) as i32 as f64,
            BinaryOperator::Equal => (a == b) as i32 as f64,
            BinaryOperator::Add => a + b,
            BinaryOperator::Sub => a - b,
            BinaryOperator::Multiply => a * b,
            BinaryOperator::Divide => if *b == 0.0 {
                f64::NAN
            } else {
                a / b
            },
            BinaryOperator::Remainder => a % b,
        };

        Some(Box::new(result))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn values_equal(&self, other: &dyn Expr) -> bool {
        other.as_any()
            .downcast_ref::<BinaryExpr>()
            .map_or(false, |expr|
                self.left.values_equal(expr.left.deref()) &&
                self.operator == expr.operator &&
                self.right.values_equal(expr.right.deref())
            )
    }
}

#[derive(Debug)]
pub(crate) struct CallExpr {
    pub line: usize,
    pub column: usize,
    pub function: String,
    pub arguments: Vec<Box<dyn Expr>>,
}

impl Expr for CallExpr {
    fn emit_bytecode(&self, env: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error> {
        for i in (0..self.arguments.len()).rev() {
            self.arguments[i].emit_bytecode(env, bc)?;
        }

        let (index, _func) = match env.function_info(&self.function) {
            Some(i) => i,
            None => return Err(Error::new(self.line, self.column, "function not found")),
        };

        bc.push(ByteCode::Call{
            func_index: index,
            num_args: self.arguments.len(),
            line: self.line,
            column: self.column,
        });
        Ok(())
    }

    fn fold(&mut self, env: &Environment) -> Option<Box<dyn Expr>> {
        for i in 0..self.arguments.len() {
            if let Some(arg) = self.arguments[i].fold(env) {
                self.arguments[i] = arg;
            }
        }

        let mut args = Vec::with_capacity(self.arguments.len());
        for arg in &self.arguments {
            match arg.as_any().downcast_ref::<f64>() {
                Some(val) if val.is_nan() => return Some(Box::new(*val)),
                Some(val) => args.push(*val),
                None => return None,
            }
        }

        let func = match env.function_info(&self.function) {
            Some((_index, func)) => func,
            None => return None, // emit_bytecode will catch the error
        };

        Some(Box::new(func(&args)))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn values_equal(&self, other: &dyn Expr) -> bool {
        other.as_any()
            .downcast_ref::<CallExpr>()
            .map_or(false, |expr| {
                if self.arguments.len() != expr.arguments.len() {
                    return false
                }

                for i in 0..self.arguments.len() {
                    if !self.arguments[i].values_equal(expr.arguments[i].deref()) {
                        return false
                    }
                }

                self.function == expr.function
            })
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct IdentifierExpr {
    pub line: usize,
    pub column: usize,
    pub identifier: String,
}

impl Expr for IdentifierExpr {
    fn emit_bytecode(&self, env: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error> {
        let index = match env.index_of_item(&self.identifier) {
            Some(i) => i,
            None => return Err(Error::new(self.line, self.column, "identifier not found")),
        };

        bc.push(ByteCode::LoadItem{ index });
        Ok(())
    }

    fn fold(&mut self, _: &Environment) -> Option<Box<dyn Expr>> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn values_equal(&self, other: &dyn Expr) -> bool {
        other.as_any()
            .downcast_ref::<IdentifierExpr>()
            .map_or(false, |expr| expr.identifier == self.identifier)
    }
}

impl Expr for f64 {
    fn emit_bytecode(&self, _: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error> {
        bc.push(ByteCode::LoadConst(*self));
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn fold(&mut self, _: &Environment) -> Option<Box<dyn Expr>> {
        None
    }

    fn values_equal(&self, other: &dyn Expr) -> bool {
        other.as_any()
            .downcast_ref::<f64>()
            .map_or(false, |val| *val == *self)
    }
}
