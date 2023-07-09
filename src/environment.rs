#[derive(Clone)]
pub enum Function {
    Single(fn(f64) -> f64),
    Double(fn(f64, f64) -> f64),
    Triple(fn(f64, f64, f64) -> f64),
    Vararg(fn(&[f64]) -> f64),
}

pub struct Environment {
    locals: Vec<String>,
    functions: Vec<(String, Function)>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            locals: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn add_local(&mut self, name: &str) {
        self.locals.push(name.to_owned());
    }

    pub fn to_input(&self, values: &[f64]) -> Input {
        Input {
            values: values.into(),
            functions: self.functions.iter().map(|(_, func)| func.clone()).collect(),
        }
    }

    pub(crate) fn index_of_item(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|s| s == name)
    }

    pub(crate) fn function_info(&self, name: &str) -> Option<(usize, &Function)> {
        self.functions.iter()
            .enumerate()
            .find(|(index, (s, func))| s == name)
            .map(|(index, (_, func))| (index, func))
    }
}

pub trait AddFunction<F> {
    fn add_function(&mut self, name: &str, f: F);
}

impl AddFunction<fn(f64, f64) -> f64> for Environment {
    fn add_function(&mut self, name: &str, f: fn(f64, f64) -> f64) {
        self.functions.push((name.to_owned(), Function::Double(f)));
    }
}

pub struct Input {
    values: Vec<f64>,
    functions: Vec<Function>,
}

impl Input {
    pub(crate) fn item(&self, index: usize) -> Option<f64> {
        self.values.get(index).copied()
    }

    pub(crate) fn function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }
}
