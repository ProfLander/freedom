use log::debug;
pub use notify_debouncer_full;
use smol::channel::Receiver;
use steel::{
    SteelVal, define_symbols, list,
    parser::interner::InternedString,
    rvals::{Custom, FromSteelVal, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    stop,
};

use std::{cell::RefCell, path::Path, rc::Rc, time::Duration};

use crate::{
    Result,
    r#async::smol::channel::unbounded,
    log::{handle_error, info},
    scheme::{steel::steelerr, steel_future},
};
use notify_debouncer_full::{
    DebounceEventResult, DebouncedEvent, FileIdMap, new_debouncer,
    notify::{
        Error, Event, EventKind, ReadDirectoryChangesWatcher,
        event::{
            AccessKind, AccessMode, CreateKind, DataChange, EventAttributes, Flag, MetadataKind,
            ModifyKind, RemoveKind, RenameMode,
        },
    },
};

pub fn watch<P, F>(
    path: P,
    timeout: Duration,
    tick_rate: Option<Duration>,
    recursive_mode: notify_debouncer_full::notify::RecursiveMode,
    mut f: F,
) -> std::result::Result<Debouncer, Error>
where
    P: AsRef<Path>,
    F: FnMut(DebouncedEvent) + 'static,
{
    let path = path.as_ref();
    debug!("watch: {path:?}");

    let (tx, rx) = unbounded();
    let mut debouncer = new_debouncer(timeout, tick_rate, move |result: DebounceEventResult| {
        handle_error(
            tx.send_blocking(result)
                .or_else(|e| steelerr!(Generic => e)),
        )
    })?;

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

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/fs");
    Watcher::register_type(&mut module);
    module
}

pub type Debouncer = notify_debouncer_full::Debouncer<ReadDirectoryChangesWatcher, FileIdMap>;

#[derive(Clone)]
pub struct Watcher {
    debouncer: Rc<RefCell<Debouncer>>,
    rx: Receiver<DebounceEventResult>,
}

impl Watcher {
    fn register_type(module: &mut BuiltInModule) {
        module
            .register_fn("Watcher", Watcher::new)
            .register_fn("Watcher-watch", Watcher::watch)
            .register_fn("Watcher-unwatch", Watcher::unwatch)
            .register_fn("Watcher-events", |watcher: Watcher| {
                steel_future(watcher.events())
            });
    }
}

#[derive(Clone)]
pub struct RecursiveMode(notify_debouncer_full::notify::RecursiveMode);

impl RecursiveMode {
    pub fn recursive() -> Self {
        RecursiveMode(notify_debouncer_full::notify::RecursiveMode::Recursive)
    }

    pub fn non_recursive() -> Self {
        RecursiveMode(notify_debouncer_full::notify::RecursiveMode::NonRecursive)
    }

    pub fn unwrap(self) -> notify_debouncer_full::notify::RecursiveMode {
        self.0
    }
}

impl IntoSteelVal for RecursiveMode {
    fn into_steelval(self) -> steel::rvals::Result<SteelVal> {
        Ok(list![
            "RecursiveMode",
            match self.0 {
                notify_debouncer_full::notify::RecursiveMode::Recursive => list!["Recursive"],
                notify_debouncer_full::notify::RecursiveMode::NonRecursive => list!["NonRecursive"],
            }
        ])
    }
}

impl FromSteelVal for RecursiveMode {
    fn from_steelval(val: &SteelVal) -> Result<RecursiveMode> {
        let SteelVal::ListV(list) = val else {
            stop!(TypeMismatch => "Expected list, got: {}", val)
        };

        let car = list.car();
        let Some(SteelVal::SymbolV(sym)) = list.car() else {
            stop!(UnexpectedToken => "Expected a symbol, got: {:?}", car)
        };

        if !matches!(sym.as_str(), "RecursiveMode") {
            stop!(UnexpectedToken => "Expected 'RecursiveMode, got: {}", sym)
        }

        let cdr = list.cdr().and_then(|cdr| cdr.car());
        let Some(SteelVal::ListV(list)) = cdr else {
            stop!(UnexpectedToken => "Expected a list, got: {:?}", cdr);
        };

        let Some(SteelVal::SymbolV(sym)) = list.car() else {
            stop!(TypeMismatch => "Expected a symbol, got: {:?}", car)
        };

        let res = match sym.as_str() {
            "Recursive" => RecursiveMode::recursive(),
            "NonRecursive" => RecursiveMode::non_recursive(),
            _ => stop!(UnexpectedToken => "Expected 'Recursive or 'NonRecursive, got: {}", sym),
        };

        Ok(res)
    }
}

pub trait FsIntoSteelVal {
    fn into_steelval(&self) -> Result<SteelVal>;
}

impl<T> FsIntoSteelVal for Option<T>
where
    T: FsIntoSteelVal,
{
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(match self {
            Some(t) => t.into_steelval()?,
            None => SteelVal::BoolV(false),
        })
    }
}

impl<T> FsIntoSteelVal for Vec<T>
where
    T: FsIntoSteelVal,
{
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(SteelVal::ListV(
            self.iter()
                .try_fold(vec![], |mut acc, next| {
                    acc.push(FsIntoSteelVal::into_steelval(next)?);
                    Ok(acc) as Result<_>
                })?
                .into(),
        ))
    }
}

macro_rules! sym {
    ($ident:ident) => {
        SteelVal::SymbolV(stringify!($ident).into())
    };
    ($str:expr) => {
        SteelVal::SymbolV($str.into())
    }
}

impl FsIntoSteelVal for DebouncedEvent {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(DebouncedEvent),
            list![
                list![sym!(event), self.event.into_steelval()?],
                list![sym!(time), self.time.into_steelval()?]
            ]
        ])
    }
}

impl FsIntoSteelVal for Event {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(Event),
            list![
                list![sym!(kind), self.kind.clone().into_steelval()?],
                list![sym!(path), self.paths.clone().into_steelval()?],
                list![sym!(attrs), self.attrs.into_steelval()?]
            ]
        ])
    }
}

impl FsIntoSteelVal for EventKind {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(EventKind),
            match self {
                EventKind::Any => list![sym!(Any)],
                EventKind::Access(access_kind) => list![sym!(Access), access_kind.into_steelval()?],
                EventKind::Create(create_kind) => list![sym!(Create), create_kind.into_steelval()?],
                EventKind::Modify(modify_kind) => list![sym!(Modify), modify_kind.into_steelval()?],
                EventKind::Remove(remove_kind) => list![sym!(Remove), remove_kind.into_steelval()?],
                EventKind::Other => todo!(),
            }
        ])
    }
}

impl FsIntoSteelVal for AccessKind {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(AccessKind),
            match self {
                AccessKind::Any => list![sym!(Any)],
                AccessKind::Read => list![sym!(Read)],
                AccessKind::Open(access_mode) => list![sym!(Open), access_mode.into_steelval()?],
                AccessKind::Close(access_mode) => list![sym!(Close), access_mode.into_steelval()?],
                AccessKind::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for AccessMode {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(AccessMode),
            match self {
                AccessMode::Any => list![sym!(Any)],
                AccessMode::Execute => list![sym!(Execute)],
                AccessMode::Read => list![sym!(Read)],
                AccessMode::Write => list![sym!(Write)],
                AccessMode::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for CreateKind {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(CreateKind),
            match self {
                CreateKind::Any => list![sym!(Any)],
                CreateKind::File => list![sym!(File)],
                CreateKind::Folder => list![sym!(Folder)],
                CreateKind::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for ModifyKind {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(ModifyKind),
            match self {
                ModifyKind::Any => list![sym!(Any)],
                ModifyKind::Data(data_change) => list![sym!(Data), data_change.into_steelval()?],
                ModifyKind::Metadata(metadata_kind) => {
                    list![sym!(Metadata), metadata_kind.into_steelval()?]
                }
                ModifyKind::Name(rename_mode) => list![sym!(Name), rename_mode.into_steelval()?],
                ModifyKind::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for DataChange {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(DataChange),
            match self {
                DataChange::Any => list![sym!(Any)],
                DataChange::Size => list![sym!(Size)],
                DataChange::Content => list![sym!(Content)],
                DataChange::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for MetadataKind {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(MetadataKind),
            match self {
                MetadataKind::Any => list![sym!(Any)],
                MetadataKind::AccessTime => list![sym!(AccessTime)],
                MetadataKind::WriteTime => list![sym!(WriteTime)],
                MetadataKind::Permissions => list![sym!(Permissions)],
                MetadataKind::Ownership => list![sym!(Ownership)],
                MetadataKind::Extended => list![sym!(Extended)],
                MetadataKind::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for RenameMode {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(RenameMode),
            match self {
                RenameMode::Any => list![sym!(Any)],
                RenameMode::To => list![sym!(To)],
                RenameMode::From => list![sym!(From)],
                RenameMode::Both => list![sym!(Both)],
                RenameMode::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for RemoveKind {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(RemoveKind),
            match self {
                RemoveKind::Any => list![sym!(Any)],
                RemoveKind::File => list![sym!(File)],
                RemoveKind::Folder => list![sym!(Folder)],
                RemoveKind::Other => list![sym!(Other)],
            }
        ])
    }
}

impl FsIntoSteelVal for EventAttributes {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(EventAttributes),
            list![
                list![sym!(tracker), self.tracker()],
                list![sym!(flag), self.flag().into_steelval()?],
                list![sym!(info), self.info()],
                list![sym!(source), self.source()],
                list![sym!("process-id"), self.process_id()]
            ]
        ])
    }
}

impl FsIntoSteelVal for Flag {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(Flag),
            match self {
                Flag::Rescan => list![sym!(Rescan)],
            }
        ])
    }
}

impl Watcher {
    pub fn new(timeout: f64, tick_rate: Option<f64>) -> Result<Self> {
        let (tx, rx) = unbounded::<DebounceEventResult>();

        let debouncer = new_debouncer(
            Duration::from_secs_f64(timeout),
            tick_rate.map(Duration::from_secs_f64),
            move |result: DebounceEventResult| {
                handle_error(tx.send_blocking(result).or_else(|e| stop!(Generic => e)))
            },
        )
        .or_else(|e| stop!(Generic => e))?;

        Ok(Watcher {
            debouncer: Rc::new(RefCell::new(debouncer)),
            rx: rx,
        })
    }

    pub fn watch(&self, path: String, recursive_mode: RecursiveMode) -> Result<SteelVal> {
        self.debouncer
            .borrow_mut()
            .watch(path, recursive_mode.unwrap())
            .or_else(|e| stop!(Generic => e))
            .into_steelval()
    }

    pub fn unwatch(&self, path: String) -> Result<SteelVal> {
        self.debouncer
            .borrow_mut()
            .unwatch(path)
            .or_else(|e| stop!(Generic => e))
            .into_steelval()
    }

    pub async fn events(self) -> Result<SteelVal> {
        self.rx
            .recv()
            .await
            .or_else(|e| stop!(Io => e))?
            .or_else(|e| stop!(Generic => "{:?}", e))?
            .into_steelval()
    }
}

impl Custom for Watcher {}
