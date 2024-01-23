use bigdecimal::{BigDecimal, One, Zero};
use rex::Environment;

fn all(args: &[BigDecimal]) -> BigDecimal {
    for arg in args {
        if arg.is_zero() {
            return BigDecimal::zero();
        }
    }

    BigDecimal::one()
}

fn main() {
    let env = rex::Environment {
        locals: vec!["one".to_owned(), "two".to_owned()],
        functions: vec![("all".to_owned(), all)],
        optimization_level: rex::OptimizationLevel::Basic,
    };

    for line in std::io::stdin().lines() {
        if let Err(e) = eval(
            &line.unwrap(),
            &env,
            &[BigDecimal::one(), BigDecimal::from(2)],
        ) {
            println!("error: {}", e,);
        }
    }
}

fn eval(mut src: &str, env: &Environment, input: &[BigDecimal]) -> Result<(), rex::Error> {
    let disassemble = if src.ends_with("#disassemble") {
        src = src.trim_end_matches("#disassemble");
        true
    } else {
        false
    };

    let expr = rex::compile(&src, &env).unwrap();
    if disassemble {
        println!("{}", expr.code_dump());
    }

    println!("= {}", expr.eval(input)?);
    Ok(())
}
