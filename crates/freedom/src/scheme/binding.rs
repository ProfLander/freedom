use steel::steel_vm::builtin::BuiltInModule;

/// Macros to simplify binding Rust libraries to a Scheme API

/// Trait for registering a type to a module
pub trait SteelType {
    fn register(module: &mut BuiltInModule);
}

#[macro_export]
macro_rules! register_methods {
    ($module:expr, $type:ty, {
        $($name:literal => $method:ident($($arg:tt)*) -> $ret:ty),*
    }) => {
        $(
            $module.register_native_fn(
                $name,
                $crate::register_methods!(@impl $type, $method, $($arg)*),
                $crate::register_methods!(@arity $($arg)*)
            );
        )*
    };

    (@impl $type:ty, $method:ident, &self) => {
        |args| {
            let foo = <$type>::from_steelval(&args[0])?;
            let bar = foo.$method()?;
            bar.into_steelval()
        }
    };

    (@impl $type:ty, $method:ident, ) => {
        |_args| <$type>::$method().into_steelval()
    };

    (@arity &self) => { Arity::Exact(1) };
    (@arity ) => { Arity::Exact(0) };
}

#[macro_export]
macro_rules! impl_steel_type {
    ($type:ty, [$($methods:tt)*]) => {
        impl $crate::scheme::binding::SteelType for $type {
            fn register(module: &mut BuiltInModule) {
                $crate::register_methods!(module, $type, { $($methods)* });
            }
        }
    };
}

#[macro_export]
macro_rules! steel_struct {
    (
        $rust_type:ty => $steel_name:ident {
            $($field_name:literal : $accessor:expr),* $(,)?
        }
    ) => {
        impl AudioIntoSteelVal for $rust_type {
            fn into_steelval(&self) -> Result<SteelVal> {
                Ok($crate::scheme::steel::list![
                    $crate::sym!($steel_name),
                    $crate::scheme::steel::list![
                        $($crate::scheme::steel::list![
                            $crate::sym!($field_name), {
                                let f: fn(&$rust_type) -> _ = $accessor;
                                f(self).into_steelval()?
                            }
                        ]),*
                    ]
                ])
            }
        }
    };
}

#[macro_export]
macro_rules! steel_enum {
    (
        $rust_type:ty => $steel_name:ident {
            $($variant:pat => $symbol:ident),* $(,)?
            @ _ => $default:ident
        }
    ) => {
        impl AudioIntoSteelVal for $rust_type {
            fn into_steelval(&self) -> Result<SteelVal> {
                Ok($crate::scheme::steel::list![
                    $crate::sym!($steel_name),
                    match self {
                        $($variant => $crate::sym!($symbol),)*
                        _ => $crate::sym!($default)
                    }
                ])
            }
        }
    };
}

#[macro_export]
macro_rules! steel_bindings {
    (
        module: $module_name:literal,

        types: {
            $($type:ty => {
                $($method_name:literal => $method:ident($($arg:tt)*) -> $ret:ty),*
            }),* $(,)?
        },

        structs: {
            $($struct_type:ty => $struct_name:ident {
                $($struct_tt:tt)*
            }),* $(,)?
        },

        enums: {
            $($enum_type:ty => $enum_name:ident {
                $($enum_tt:tt)*
            }),* $(,)?
        }
    ) => {
        // Generate implementations for each struct
        $($crate::steel_struct!($struct_type => $struct_name {
            $($struct_tt)*
        });)*

        // Generate implementations for each enum
        $($crate::steel_enum!($enum_type => $enum_name {
            $($enum_tt)*
        });)*

        // Generate implementations for each type
        $(
            $crate::impl_steel_type!($type, [
                $($method_name => $method($($arg)*) -> $ret),*
            ]);
        )*

        // Generate module function
        pub fn module() -> $crate::scheme::steel::steel_vm::builtin::BuiltInModule {
            let mut module = $crate::scheme::steel::steel_vm::builtin::BuiltInModule::new($module_name);
            $(
                <$type as $crate::scheme::binding::SteelType>::register(&mut module);
            )*
            module
        }
    };
}
