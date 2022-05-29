
Provides extension to rust enum

This crate provides `Enumeration` trait for rust `enum` with the following features 
- implementation for common traits (Clone, Copy, Hash, etc.)
- getting number of variants through constant `Enumeration::VARIANT_COUNT`
- casting between index (of type `Enumeration::Index`) and enumeration
- attaching a constant value to each of the variants