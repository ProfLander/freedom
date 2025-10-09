use freedom_core::plugins::plugin::{FunctionSpec, PluginSpec};
use steel::steel_vm::builtin::Arity;

#[unsafe(no_mangle)]
pub fn print_hello() {
    println!("Hello, wauw!");
}

#[unsafe(no_mangle)]
pub fn spec() -> PluginSpec {
    PluginSpec {
        name: "library",
        values: &[],
        functions: &[FunctionSpec {
            symbol: "print_hello",
            name: "print-hello",
            arity: Arity::Exact(0),
        }],
    }
}
