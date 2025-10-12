#[cfg(feature = "debug")]
static PLUGIN_DIR: &str = "target/debug/deps";

#[cfg(feature = "release")]
static PLUGIN_DIR: &str = "plugins";

static SCRIPT_DIR: &str = "scheme";
static ENTRYPOINT: &str = "main.scm";

// Entrypoint
fn main() {
    freedom::run(PLUGIN_DIR, SCRIPT_DIR, ENTRYPOINT)
}
