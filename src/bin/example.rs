use rex::Function;

fn all(a: f64, b: f64) -> f64 {
    if a == 1. && b == 1. {
        1.
    } else {
        0.
    }
}

fn main() {
    let mut env = rex::Environment::new(&["one", "two"]);
    env.add_function("and", Function::Double(all));

    for line in std::io::stdin().lines() {
        let expr = rex::compile(&line.unwrap(), &env).unwrap();

        match expr.eval(&[1.0, 2.0]) {
            Ok(val) => println!("= {}", val),
            Err(e) => println!("error: {}", e,),
        }
    }
}
