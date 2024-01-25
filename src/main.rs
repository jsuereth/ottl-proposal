pub mod ast;
pub mod parser;
pub mod typer;
pub mod ir;
pub mod transform;
pub mod compiler;


fn main() {

    for filename in std::env::args().skip(1) {
        println!("Transpiling {}!", filename);
        let input = std::fs::read_to_string(filename).expect("Unable to find the file!");
        let result = compiler::transpile(&input).expect("Compilation failed!");
        println!("--------");
        println!("{}", result);
        println!("--------");
    }
    
    
}
