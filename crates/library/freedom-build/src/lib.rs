use std::env;

/// Enables a feature corresponding to the active rustc profile
///
/// i.e. `debug` for debug builds, `release` for release builds, etc.
pub fn profile_feature() {
    if let Ok(profile) = env::var("PROFILE") {
        println!("cargo:rustc-cfg=feature={:?}", profile);
    }
}
