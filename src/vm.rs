use crate::environment::Environment;
use crate::error::Error;
use crate::expression::ByteCode;

pub(crate) fn eval(bc: &[ByteCode], input: &[f64], env: &Environment) -> Result<f64, Error> {
    let mut stack = Stack(Vec::with_capacity(32));
    let mut pc = 0;

    loop {
        let op = match bc.get(pc) {
            Some(op) => op,
            None => break,
        };

        use ByteCode::*;
        match op {
            LoadConst(val) => stack.push(*val),
            LoadItem { index } => {
                let val = match input.get(*index) {
                    Some(v) => *v,
                    None => return Err(Error::new(0, 0, "VM Error")),
                };

                stack.push(val);
            }
            Call {
                func_index,
                num_args,
                line,
                column,
            } => {
                let function = match env.get_function(*func_index) {
                    Some(f) => f,
                    None => return Err(Error::new(0, 0, "VM Error")),
                };

                let mut args = Vec::with_capacity(*num_args);
                for _ in 0..*num_args {
                    args.push(stack.pop()?);
                }

                let result = function(&args);
                if result.is_nan() {
                    return Err(Error::new(*line, *column, "function returned Nan"));
                }

                stack.push(result);
            }
            LessThan => {
                stack.cmp(|a, b| a < b)?;
            }
            LessEqual => {
                stack.cmp(|a, b| a <= b)?;
            }
            GreaterEqual => {
                stack.cmp(|a, b| a >= b)?;
            }
            GreaterThan => {
                stack.cmp(|a, b| a > b)?;
            }
            Equal => {
                stack.cmp(|a, b| a == b)?;
            }
            Add => stack.math(|a, b| a + b)?,
            Sub => stack.math(|a, b| a - b)?,
            Multiply => stack.math(|a, b| a * b)?,
            Divide => stack.math(|a, b| a / b)?,
            Remainder => stack.math(|a, b| a % b)?,
            JumpIfZero { offset } => {
                if stack.pop()? == 0.0 {
                    pc += *offset;
                }
            }
            Jump { offset } => pc += *offset,
        }

        pc += 1;
    }

    if stack.len() != 1 {
        return Err(Error::new(
            0,
            0,
            &format!(
                "VM ERROR: expected 1 value on top of stack but had {}",
                stack.len()
            ),
        ));
    }

    Ok(stack.pop()?)
}

struct Stack(Vec<f64>);

impl Stack {
    fn push(&mut self, val: f64) {
        self.0.push(val);
    }

    fn pop(&mut self) -> Result<f64, Error> {
        self.0.pop().ok_or(Error::new(0, 0, "VM Error"))
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn cmp<F: Fn(f64, f64) -> bool>(&mut self, f: F) -> Result<(), Error> {
        if f(self.pop()?, self.pop()?) {
            self.push(1.);
        } else {
            self.push(0.);
        }
        Ok(())
    }

    fn math<F: Fn(f64, f64) -> f64>(&mut self, f: F) -> Result<(), Error> {
        let result = f(self.pop()?, self.pop()?);
        self.push(result);
        Ok(())
    }
}
