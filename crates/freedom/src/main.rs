use freedom::scheme::SchemeConfig;

static KERNEL: &str = "scheme/kernel.scm";
static MAIN: &str = "scheme/main.scm";
static WORKER: &str = "scheme/worker.scm";

// Entrypoint
fn main() {
    freedom::run(
        SchemeConfig {
            kernel: KERNEL.to_string(),
        },
        MAIN,
        WORKER,
    )
}
