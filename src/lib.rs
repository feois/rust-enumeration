#![warn(missing_docs)]

//! Provides extension to rust enum
//!
//! This crate provides [`crate::enumerate`] macro to generate rust `enum` with the following features
//! - implementation for common traits ([`Clone`], [`Copy`], [`Hash`], etc.)
//! - implementation of the useful trait [`enumeration::Enumeration`]
//! - getting number of variants through constant [`enumeration::Enumeration::VARIANT_COUNT`]
//! - casting to and from enumeration variant and enumeration index of type [`enumeration::Enumeration::Index`]
//! - associating a constant value with each of the variants (with default value support [`enumeration::DefaultAssociatedValue`])
//! - iterating through all variants [`enumeration::Enumeration::iter`]
//! - two-way mapping for the variants and the associated constants [`enumeration::FromValue`]
//! - bit-masking support
//!     - wrapper that redefines some operators for greater bit-masking related bit operations
//! - limited dynamic dispatch feature
//!     - allow different [`enumeration::Enumeration<Index = T>`] to cast into [`variant::Variant<T>`]

pub mod bitmask;
pub mod enumeration;
pub mod helper;
pub mod variant;

#[doc(hidden)]
mod macros;

/// Convenience re-export of common members
pub mod prelude {
    pub use crate::enumerate;
    pub use crate::{enumeration::*, helper::*};
}
