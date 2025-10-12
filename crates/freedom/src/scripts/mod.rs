pub mod scripts;

use std::{
    borrow::Cow,
    ffi::{OsStr, OsString},
};

use crate::{
    log::info,
    scheme::{
        Result,
        program::Program,
        steel::{
            SteelVal,
            steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
        },
    },
};

use log::debug;
use scripts::Scripts;
use steel::{steelerr, throw};

thread_local! {
    static SCRIPTS: Scripts = Scripts::new();
}

pub fn init<P: AsRef<OsStr>>(path: &P) -> Result<()> {
    SCRIPTS.with(|scripts| scripts.watch(path))?;
    Ok(())
}

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/scripts");
    module.register_fn("#%get-script", |name: String| {
        get_script(OsString::from(name))
    });
    module
}

pub async fn run<P: AsRef<OsStr>>(name: &P) -> Result<Vec<SteelVal>> {
    debug!(
        "Running {}",
        name.as_ref().to_str().ok_or_else(
            throw!(Generic => "Failed to convert path to string slice: {:?}", name.as_ref())
        )?
    );
    let main = get_script(name).await?;
    crate::scheme::with_engine_mut(|engine| engine.run_raw_program(main.unwrap()))
}

pub async fn get_script<P: AsRef<OsStr>>(name: P) -> Result<Program> {
    let prog = SCRIPTS.with(|scripts| {
        let prog = if let Some(entry) = scripts.get(&name) {
            entry.clone()
        } else {
            let script = scripts.load_script(&name)?;

            scripts.insert(name.as_ref().to_os_string(), script.clone());
            script
        };
        Ok(Program::new(prog)) as Result<_>
    })?;

    Ok(prog)
}
