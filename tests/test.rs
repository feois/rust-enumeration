use std::any::{Any, TypeId};

use enumeration::prelude::*;
use enumeration::variant::*;

enumerate!(Test(u8; i32 = 99)
    X = 111
    Y
    Z = 777
);

enumerate!(TestUnitType(u8; () = ())
    Foo
    Bar = { assert!(true); }
);

enumerate!(Str(u8; &'static str)
    HelloWorld = "Hello world!"
);

#[test]
fn test_index() {
    assert_eq!(Test::X.to_index(), 0);
    assert_eq!(Test::Y.to_index(), 1);
    assert_eq!(Test::Z.to_index(), 2);
}

#[test]
fn test_cast() {
    assert_eq!(Test::variant(0).unwrap(), Test::X);
    assert_eq!(Test::variant(1).unwrap(), Test::Y);
    assert_eq!(Test::variant(2).unwrap(), Test::Z);
}

#[test]
fn test_associated_constant_values() {
    assert_eq!(*Test::X.value(), 111);
    assert_eq!(*Test::Y.value(), 99);
    assert_eq!(*Test::Z.value(), 777);
}

#[test]
fn test_string() {
    assert_eq!(*Str::HelloWorld.value(), "Hello world!");
}

#[test]
fn test_variant() {
    let vec: Vec<Variant<_>> = vec![Test::X.into(), Str::HelloWorld.into()];
    
    assert_eq!(vec[0].cast::<Test>(), Ok(Test::X));
    assert_eq!(vec[1].cast::<Str>(), Ok(Str::HelloWorld));
    assert!(vec[1].cast::<Test>().is_err());
}

#[test]
fn test_variant_with() {
    let vec: Vec<VariantWith<_, _>> = vec![Test::X.into(), Test::Z.into()];
    
    assert_eq!(*vec[0].value(), 111);
    assert_eq!(*vec[1].value(), 777);
}

#[test]
fn test_iter() {
    let mut iter = (0..Test::VARIANT_COUNT).variants();

    assert_eq!(iter.next(), Some(Ok(Test::X)));
    assert_eq!(iter.next(), Some(Ok(Test::Y)));
    assert_eq!(iter.next(), Some(Ok(Test::Z)));
    assert_eq!(iter.next(), None);

    let mut iter = (Test::VARIANT_COUNT..=Test::VARIANT_COUNT).variants::<Test>();

    assert_eq!(iter.next(), Some(Err(OutOfRangeError(Test::VARIANT_COUNT))));
}

#[test]
fn test_iter_unchecked() {
    let mut iter = unsafe { (0..Test::VARIANT_COUNT).variants_unchecked() };

    assert_eq!(iter.next(), Some(Test::X));
    assert_eq!(iter.next(), Some(Test::Y));
    assert_eq!(iter.next(), Some(Test::Z));
    assert_eq!(iter.next(), None);
}

#[cfg(debug_assertions)]
#[test]
#[should_panic]
fn test_iter_unchecked_fail() {
    (Test::VARIANT_COUNT..=Test::VARIANT_COUNT).variants_unwrap::<Test>().next();
}

#[test]
fn test_iter_unwrap() {
    let mut iter = (0..Test::VARIANT_COUNT).variants_unwrap();

    assert_eq!(iter.next(), Some(Test::X));
    assert_eq!(iter.next(), Some(Test::Y));
    assert_eq!(iter.next(), Some(Test::Z));
    assert_eq!(iter.next(), None);
}

#[test]
#[should_panic]
fn test_iter_unwrap_fail() {
    (Test::VARIANT_COUNT..=Test::VARIANT_COUNT).variants_unwrap::<Test>().next();
}

#[test]
fn test_len() {
    assert_eq!(Test::count(), Test::VARIANT_COUNT);
    assert_eq!(Test::len(), Test::count() as usize);
    assert_eq!(Test::len().type_id(), TypeId::of::<usize>())
}
