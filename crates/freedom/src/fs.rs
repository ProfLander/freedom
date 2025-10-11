pub use notify_debouncer_full;

use std::{path::Path, time::Duration};

use crate::{
    r#async::smol::channel::unbounded,
    log::{handle_error, info},
    scheme::steel::steelerr,
};
use notify_debouncer_full::{
    DebounceEventResult, DebouncedEvent, Debouncer, FileIdMap, RecommendedCache, new_debouncer_opt,
    notify::{Config, Error, ReadDirectoryChangesWatcher, RecursiveMode},
};

pub type Watcher = Debouncer<ReadDirectoryChangesWatcher, FileIdMap>;

pub fn watch<P, F>(
    path: P,
    timeout: Duration,
    tick_rate: Option<Duration>,
    recursive_mode: RecursiveMode,
    mut f: F,
) -> Result<Watcher, Error>
where
    P: AsRef<Path>,
    F: FnMut(DebouncedEvent) + 'static,
{
    let path = path.as_ref();
    info!("watch: {path:?}");

    let (tx, rx) = unbounded();
    let mut debouncer = new_debouncer_opt(
        timeout,
        tick_rate,
        move |result: DebounceEventResult| {
            handle_error(
                tx.send_blocking(result)
                    .or_else(|e| steelerr!(Generic => e)),
            )
        },
        RecommendedCache::new(),
        Config::default()
            .with_follow_symlinks(true)
            .with_compare_contents(true)
            .with_manual_polling(),
    )?;

    crate::r#async::executor().spawn::<_, ()>(async move {
        loop {
            let events = rx.recv().await;
            let events = events.or_else(|e| steelerr!(Generic => e))?;
            let events = events.or_else(|e| steelerr!(Generic => "{:?}", e))?;
            for event in events {
                f(event);
            }
        }
    });

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    debouncer.watch(path, recursive_mode)?;
    Ok(debouncer)
}
