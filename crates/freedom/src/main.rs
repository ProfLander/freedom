use freedom::scheme::SchemeConfig;

static KERNEL: &str = "(load \"scheme/kernel.scm\")";
static MAIN: &str = "(load \"scheme/main.scm\")";

// Entrypoint
fn main() {
    freedom::run(
        SchemeConfig {
            kernel: KERNEL.to_string(),
        },
        MAIN,
    )
}
