use crate::environment::{Environment, Function};
use crate::error::Error;

#[derive(Clone)]
pub(crate) enum ByteCode {
    LoadConst(f64),
    LoadItem{ index: usize },
    Call{ func_index: usize, line: usize, column: usize },
    CallVararg{ func_index: usize, num_args: usize, line: usize, column: usize },
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

pub(crate) trait Expr {
    fn emit_bytecode(&self, env: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error>;
}

pub(crate) type BoxedExpr = Box<dyn Expr>;

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
}

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
}

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

        let (index, func) = match env.function_info(&self.function) {
            Some(i) => i,
            None => return Err(Error::new(self.line, self.column, "function not found")),
        };

        let expected_args = match func {
            Function::Single(_) => 1,
            Function::Double(_) => 2,
            Function::Triple(_) => 3,
            Function::Vararg(_) => {
                bc.push(ByteCode::CallVararg{
                    func_index: index,
                    num_args: self.arguments.len(),
                    line: self.line,
                    column: self.column,
                });
                return Ok(())
            },
        };

        if expected_args != 0 && self.arguments.len() != expected_args {
            return Err(Error::new(self.line, self.column, "invalid num args"))
        }

        bc.push(ByteCode::Call{func_index: index, line: self.line, column: self.column});
        Ok(())
    }
}

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
}

impl Expr for f64 {
    fn emit_bytecode(&self, _: &Environment, bc: &mut Vec<ByteCode>) -> Result<(), Error> {
        bc.push(ByteCode::LoadConst(*self));
        Ok(())
    }
}
