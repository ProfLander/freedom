use std::path::{Path, PathBuf};

use log::{debug, info};
use steel::{
    SteelVal,
    rvals::{Custom, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    steelerr, throw,
};

use crate::{
    Result,
    log::{handle_error, handle_error_with},
};

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/tempfile");
    module
        .register_fn("TempFile", TempFile::new::<String>)
        .register_fn("TempFile-src", TempFile::src)
        .register_fn("TempFile-path", TempFile::path);
    module
}

// Handle to a temporary copy of a file
#[derive(Debug)]
pub struct TempFile {
    src: PathBuf,
    path: PathBuf,
}

impl Custom for TempFile {}

fn gen_id<P: AsRef<str>>(name: P) -> String {
    uuid::Uuid::new_v4().to_string() + "-" + name.as_ref()
}

impl TempFile {
    pub fn new<P: AsRef<Path>>(src: P) -> Result<SteelVal> {
        let src: &Path = src.as_ref();
        debug!("TempFile::new({src:?})");

        let mut dest = PathBuf::from(std::env::temp_dir());
        handle_error_with(|| {
            Ok(dest.push(gen_id(
                src.file_name()
                    .ok_or_else(throw!(Io => "Path has no filename: {:?}", src))?
                    .to_string_lossy()
                    .to_string(),
            )))
        });

        debug!("Copying to tempfile at {dest:?}...");
        std::fs::copy(&src, &dest)?;

        TempFile {
            src: src.to_path_buf(),
            path: dest,
        }
        .into_steelval()
    }

    pub fn src(&self) -> String {
        self.src.to_string_lossy().to_string()
    }

    pub fn path(&self) -> String {
        self.path.to_string_lossy().to_string()
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        info!("Removing tempfile at {:?}...", self.path);
        handle_error(std::fs::remove_file(&self.path).or_else(|e| steelerr!(Io => e)))
    }
}
