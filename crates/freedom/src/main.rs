static PLUGIN_DIR: &str = "target/debug/deps";
static SCRIPT_DIR: &str = "scheme";
static ENTRYPOINT: &str = "main";

// Entrypoint
fn main() {
    freedom::run(PLUGIN_DIR, SCRIPT_DIR, ENTRYPOINT)
}
