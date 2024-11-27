use std::any::{Any, TypeId};

use enumeration::prelude::*;
use enumeration::variant::*;

enumerate!(pub TestPub(u8)
    Foo
);

enumerate!(enum TestEnum(u8)
    Foo
);

enumerate!(pub enum TestPubEnum(u8)
    Foo
);

const DEFAULT_STR: &'static str = "default";
const FOO_STR: &'static str = "foo";
const BAZ_STR: &'static str = "baz";

enumerate!(Test(u8; &'static str = DEFAULT_STR)
    Foo = FOO_STR
    Bar
    Baz = BAZ_STR
);

enumerate!(TestUnitType(u8; () = ())
    Foo
    Bar = { assert!(true); }
);

const HELLO_WORLD_STR: &'static str = "Hello world!";

enumerate!(TestString(u8; &'static str)
    HelloWorld = HELLO_WORLD_STR
);

enumerate!(TestUsize(usize)
    Foo
    Bar
    Baz
);

enumerate!(TestSameValue(u8; char)
    Foo = 'a'
    Bar = 'a'
    Baz = 'a'
);

#[test]
fn test_index() {
    assert_eq!(Test::Foo.to_index(), 0);
    assert_eq!(Test::Bar.to_index(), 1);
    assert_eq!(Test::Baz.to_index(), 2);
}

#[test]
fn test_cast() {
    assert_eq!(Test::variant(0).unwrap(), Test::Foo);
    assert_eq!(Test::variant(1).unwrap(), Test::Bar);
    assert_eq!(Test::variant(2).unwrap(), Test::Baz);
}

#[test]
fn test_associated_constant_values() {
    assert_eq!(*Test::Foo.value(), FOO_STR);
    assert_eq!(*Test::Bar.value(), DEFAULT_STR);
    assert_eq!(*Test::Baz.value(), BAZ_STR);
}

#[test]
fn test_string() {
    assert_eq!(*TestString::HelloWorld.value(), HELLO_WORLD_STR);
}

#[test]
fn test_variant() {
    let vec: Vec<Variant<_>> = vec![Test::Foo.into(), TestString::HelloWorld.into()];
    
    assert_eq!(vec[0].cast::<Test>(), Ok(Test::Foo));
    assert_eq!(vec[1].cast::<TestString>(), Ok(TestString::HelloWorld));
    assert!(vec[1].cast::<Test>().is_err());
}

#[test]
fn test_variant_with() {
    let vec: Vec<VariantWith<_, _>> = vec![Test::Foo.into(), Test::Baz.into()];
    
    assert_eq!(*vec[0].value(), FOO_STR);
    assert_eq!(*vec[1].value(), BAZ_STR);
}

#[test]
fn test_iter() {
    let mut iter = (0..Test::VARIANT_COUNT).variants();

    assert_eq!(iter.next(), Some(Ok(Test::Foo)));
    assert_eq!(iter.next(), Some(Ok(Test::Bar)));
    assert_eq!(iter.next(), Some(Ok(Test::Baz)));
    assert_eq!(iter.next(), None);

    let mut iter = (Test::VARIANT_COUNT..=Test::VARIANT_COUNT).variants::<Test>();

    assert_eq!(iter.next(), Some(Err(OutOfRangeError(Test::VARIANT_COUNT))));
}

#[test]
fn test_iter_unchecked() {
    let mut iter = unsafe { (0..Test::VARIANT_COUNT).variants_unchecked() };

    assert_eq!(iter.next(), Some(Test::Foo));
    assert_eq!(iter.next(), Some(Test::Bar));
    assert_eq!(iter.next(), Some(Test::Baz));
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

    assert_eq!(iter.next(), Some(Test::Foo));
    assert_eq!(iter.next(), Some(Test::Bar));
    assert_eq!(iter.next(), Some(Test::Baz));
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

#[test]
fn test_try_iter() {
    let mut iter = Test::try_iter();

    assert_eq!(iter.next(), Some(Test::Foo));
    assert_eq!(iter.next(), Some(Test::Bar));
    assert_eq!(iter.next(), Some(Test::Baz));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_iter_usize() {
    let mut iter = TestUsize::iter();

    assert_eq!(iter.next(), Some(TestUsize::Foo));
    assert_eq!(iter.next(), Some(TestUsize::Bar));
    assert_eq!(iter.next(), Some(TestUsize::Baz));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_from_value() {
    assert_eq!(Test::from_value(&FOO_STR), Ok(Test::Foo));
    assert_eq!(Test::from_value(&DEFAULT_STR), Ok(Test::Bar));
    assert_eq!(Test::from_value(&BAZ_STR), Ok(Test::Baz));
    assert!(Test::from_value(&"").is_err());
}

#[test]
fn test_from_value_unchecked() {
    assert_eq!(Test::from_value_unchecked(&FOO_STR), Test::Foo);
    assert_eq!(Test::from_value_unchecked(&DEFAULT_STR), Test::Bar);
    assert_eq!(Test::from_value_unchecked(&BAZ_STR), Test::Baz);
}

#[test]
#[should_panic]
fn test_from_value_unchecked_fail() {
    Test::from_value_unchecked(&"");
}

#[test]
fn test_same_value() {
    assert_eq!(TestSameValue::from_value_unchecked(&'a'), TestSameValue::Foo);
}

#[test]
fn test_default() {
    assert_eq!(DEFAULT_STR, Test::DEFAULT_ASSOCIATED_VALUE);
}
