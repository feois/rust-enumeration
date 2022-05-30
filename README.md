
Provides extension to rust enum

This crate provides `Enumeration` trait for rust `enum` with the following features 
- implementation for common traits (Clone, Copy, Hash, etc.)
- getting number of variants through constant `Enumeration::VARIANT_COUNT`
- casting between index (of type `Enumeration::Index`) and enumeration
- attaching a constant value to each of the variants
- runtime representation of enumeration

```rust
use enumeration::{prelude::*, OutOfRangeError};

enumerate!(pub Foo(u8; i32 = 11)
    Bar
    Baz = 99
);

pub fn main() -> Result<Foo, OutOfRangeError> {
    assert_eq!(Foo::Bar, Foo::variant(0)?);
    assert_eq!(Foo::Baz.to_index(), 1);
    assert_eq!(Foo::Bar.value(), 11);
    assert_eq!(Foo::Baz.value(), 99);
    
    let variants: Vec<Variant<_>> = vec![Foo::Bar.into(), SomeOtherExternalEnum::SomeVariant.into()];
    
    assert_eq!(variants[0].cast::<Foo>(), Ok(Foo::Bar));
    assert_eq!(variants[1].cast::<SomeOtherExternalEnum>(), Ok(SomeOtherExternalEnum::SomeVariant));
    assert!(variants[1].cast::<Foo>().is_err());
}
```
