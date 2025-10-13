use steel::{
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    steelerr,
};

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/fs");
    module
        .register_fn("delete-file!", |path: String| {
            std::fs::remove_file(path).or_else(|e| steelerr!(Io => e))
        })
        .register_fn("copy-file!", |from: String, to: String| {
            std::fs::copy(from, to).or_else(|e| steelerr!(Io => e))
        })
        .register_fn("temp-dir", || {
            std::env::temp_dir().to_string_lossy().to_string()
        })
        .register_fn("unique-id", || uuid::Uuid::new_v4().to_string());
    module
}
