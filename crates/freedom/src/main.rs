use freedom::scheme::SchemeConfig;

static KERNEL: &str = "scheme/kernel.scm";
static ENTRYPOINT: &str = "scheme/main.scm";

// Entrypoint
fn main() {
    freedom::run(
        SchemeConfig {
            kernel: KERNEL.to_string(),
        },
        ENTRYPOINT,
    )
}
