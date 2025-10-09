use std::{path::Path, time::Duration};

use log::info;
use notify_debouncer_full::{
    DebounceEventResult, DebouncedEvent, Debouncer, FileIdMap, RecommendedCache, new_debouncer_opt,
    notify::{Config, Error, ReadDirectoryChangesWatcher, RecursiveMode},
};
use smol::channel::unbounded;
use steel::{steelerr};

use crate::{r#async::EXECUTOR, handle_error, handle_error_async};

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

    EXECUTOR.with(|exe| {
        exe.borrow()
            .spawn(handle_error_async::<_, ()>(async move {
                loop {
                    let events = rx.recv().await;
                    let events = events.or_else(|e| steelerr!(Generic => e))?;
                    let events = events.or_else(|e| steelerr!(Generic => "{:?}", e))?;
                    for event in events {
                        f(event);
                    }
                }
            }))
            .detach()
    });

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    debouncer.watch(path, recursive_mode)?;
    Ok(debouncer)
}
