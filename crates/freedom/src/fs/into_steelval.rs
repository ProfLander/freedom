pub use notify_debouncer_full;
use steel::{SteelVal, list, rvals::IntoSteelVal};

use crate::{Result, sym};
use notify_debouncer_full::{
    DebouncedEvent,
    notify::{
        Event, EventKind,
        event::{
            AccessKind, AccessMode, CreateKind, DataChange, EventAttributes, Flag, MetadataKind,
            ModifyKind, RemoveKind, RenameMode,
        },
    },
};

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
