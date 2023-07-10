#[derive(Clone)]
pub enum Function {
    Single(fn(f64) -> f64),
    Double(fn(f64, f64) -> f64),
    Triple(fn(f64, f64, f64) -> f64),
    Vararg(fn(&[f64]) -> f64),
}

impl Into<Function> for fn(f64, f64) -> f64 {
    fn into(self) -> Function {
        Function::Double(self)
    }
}

pub struct Environment {
    locals: Vec<String>,
    functions: Vec<(String, Function)>,
}

impl Environment {
    pub fn new(locals: &[&str]) -> Environment {
        Environment {
            locals: locals.iter().map(|s| (*s).to_owned()).collect(),
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, name: &str, func: Function) {
        self.functions.push((name.to_owned(), func));
    }

    pub(crate) fn index_of_item(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|s| s == name)
    }

    pub(crate) fn function_info(&self, name: &str) -> Option<(usize, &Function)> {
        self.functions.iter()
            .enumerate()
            .find(|(_index, (s, _func))| s == name)
            .map(|(index, (_, func))| (index, func))
    }

    pub(crate) fn get_function(&self, func_index: usize) -> Option<&Function> {
        self.functions.get(func_index).map(|(_name, func)| func)
    }
}
