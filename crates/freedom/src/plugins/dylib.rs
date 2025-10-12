use std::{
    mem::ManuallyDrop,
    ops::Deref,
    path::{Path, PathBuf},
};

use libloading::Library;
use log::debug;

use crate::scheme::{
    Result,
    steel::{rvals::Custom, steelerr, throw},
};
use crate::{
    err,
    log::{handle_error, handle_error_with},
};

// Handle to a temporary copy of a dynamic library
#[derive(Debug)]
pub struct Dylib {
    lib: ManuallyDrop<Library>,
    path: PathBuf,
}

impl Deref for Dylib {
    type Target = Library;

    fn deref(&self) -> &Self::Target {
        &self.lib
    }
}

impl Custom for Dylib {}

impl Dylib {
    pub fn new<P: AsRef<Path>>(src: &P) -> Result<Self> {
        let src: &Path = src.as_ref();
        debug!("Dylib::new({src:?})");

        let mut dest = PathBuf::from(std::env::temp_dir());
        handle_error_with(|| {
            Ok(dest.push(
                uuid::Uuid::new_v4().to_string()
                    + "-"
                    + src
                        .file_name()
                        .ok_or_else(throw!(Io => "Path has no filename: {:?}", src))?
                        .to_str()
                        .ok_or_else(
                            || err!(Io => "Failed to convert path to string slice: {:?}", src),
                        )?,
            ))
        });

        debug!("Copying to tempfile at {dest:?}...");
        std::fs::copy(&src, &dest)?;

        debug!("Loading...");
        let lib = unsafe { Library::new(&dest).or_else(|e| steelerr!(Io => e))? };
        let lib = ManuallyDrop::new(lib);

        Ok(Dylib { path: dest, lib })
    }
}

impl Drop for Dylib {
    fn drop(&mut self) {
        debug!("Dropping library handle...");
        unsafe { ManuallyDrop::drop(&mut self.lib) };

        debug!("Removing tempfile at {:?}...", self.path);
        handle_error(std::fs::remove_file(&self.path).or_else(|e| steelerr!(Io => e)))
    }
}
