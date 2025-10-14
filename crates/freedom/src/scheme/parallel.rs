use std::{cell::RefCell, rc::Rc, thread::JoinHandle};

use steel::{
    SteelVal,
    primitives::lists::plist_get,
    rvals::{Custom, FromSteelVal, IntoSteelVal, SerializableSteelVal},
    steel_vm::{
        builtin::{Arity, BuiltInModule},
        register_fn::RegisterFn,
    },
    steelerr, throw,
};

use crate::{Result, scheme::SchemeConfig, sym};

#[derive(Clone)]
pub struct Thread {
    handle: Rc<RefCell<Option<JoinHandle<()>>>>,
}

impl Thread {
    pub fn new(
        config: SchemeConfig,
        entrypoint: SteelVal,
        name: Option<String>,
        stack_size: Option<usize>,
    ) -> Result<Self> {
        let mut builder = std::thread::Builder::new();
        if let Some(name) = name {
            builder = builder.name(name);
        }

        if let Some(stack_size) = stack_size {
            builder = builder.stack_size(stack_size);
        }

        let sender = std::sync::mpsc::Sender::<SerializableSteelVal>::from_steelval(&entrypoint)?;

        let handle = Rc::new(RefCell::new(Some(
            builder
                .spawn(move || {
                    crate::worker_thread_run_with(config, |engine| {
                        engine.register_value("*tx*", sender.into_steelval()?);
                        engine.run(
                            "
                            (define channels (make-channels))
                            (define tx (car channels))
                            (define rx (cadr channels))
                            (channel->send *tx* tx)
                            (define f (channel->recv rx))
                            (info! \"f:\" f)
                            (f)
                        ",
                        )
                    })
                })
                .or_else(|e| steelerr!(Generic => e))?,
        )));

        Ok(Thread { handle })
    }

    pub fn join(&self) -> Result<SteelVal> {
        self.handle
            .borrow_mut()
            .take()
            .ok_or_else(throw!(Generic => "Thread has already been joined"))?
            .join()
            .or_else(|_| steelerr!(Generic => "Failed to join thread"))?;
        Ok(SteelVal::Void)
    }
}

impl Custom for Thread {}

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/parallel");

    module.register_native_fn(
        "Thread",
        |args| {
            let config = SchemeConfig::from_steelval(&args[0])?;
            let entrypoint = args[1].clone();

            let kwargs = args[2..].iter().cloned().collect();

            let name = plist_get(&kwargs, &sym!("#:name"))
                .ok()
                .map(|t| String::from_steelval(&t))
                .transpose()?;

            let stack_size = plist_get(&kwargs, &sym!("#:stack-size"))
                .ok()
                .map(|t| usize::from_steelval(&t))
                .transpose()?;

            Thread::new(config, entrypoint, name, stack_size).into_steelval()
        },
        Arity::AtLeast(2),
    );
    module.register_fn("Thread-join", Thread::join);

    module
}
