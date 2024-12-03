use std::any::{Any, TypeId};

use enumeration::bitmask::*;
use enumeration::prelude::*;
use enumeration::variant::*;

enumerate! {
    pub TestPub(u8)
        Foo
}

enumerate! {
    enum TestEnum(u8)
        Foo
}

enumerate! {
    pub enum TestPubEnum(u8)
        Foo
}

const DEFAULT_STR: &'static str = "default";
const FOO_STR: &'static str = "foo";
const BAZ_STR: &'static str = "baz";

enumerate! {
    Test(u8; &'static str = DEFAULT_STR)
        Foo = FOO_STR
        Bar
        Baz = BAZ_STR
}

enumerate! {
    TestUnitType(u8; () = ())
        Foo
        Bar = { assert!(true); }
}

const HELLO_WORLD_STR: &'static str = "Hello world!";

enumerate! {
    TestString(u8; &'static str)
        HelloWorld = HELLO_WORLD_STR
}

enumerate! {
    TestUsize(usize)
        Foo
        Bar
        Baz
}

enumerate! {
    TestSameValue(u8; char)
        Foo = 'a'
        Bar = 'a'
        Baz = 'a'
}

enumerate! {
    one-way TestOneWay(u8; fn())
        Foo = (|| ()) as fn()
}

enumerate! {
    TestAlias(u8; alias: char)
        Foo = 'a'
        Bar = 'b'
}

bit_enum! {
    TestBitEnum (u8; u16)
        Foo
        Bar
        Baz
}

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
    (Test::VARIANT_COUNT..=Test::VARIANT_COUNT)
        .variants_unwrap::<Test>()
        .next();
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
    (Test::VARIANT_COUNT..=Test::VARIANT_COUNT)
        .variants_unwrap::<Test>()
        .next();
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
    assert_eq!(
        TestSameValue::from_value_unchecked(&'a'),
        TestSameValue::Foo
    );
}

#[test]
fn test_default() {
    assert_eq!(DEFAULT_STR, Test::DEFAULT_ASSOCIATED_VALUE);
}

#[test]
fn test_bit_enum() {
    assert_eq!(TestBitEnum::Foo.bit(), Bits(1 << 0));
    assert_eq!(TestBitEnum::Bar.bit(), Bits(1 << 1));
    assert_eq!(TestBitEnum::Baz.bit(), Bits(1 << 2));
    assert_eq!(TestBitEnum::from_bit(1 << 0), Some(TestBitEnum::Foo));
    assert_eq!(TestBitEnum::from_bit(1 << 1), Some(TestBitEnum::Bar));
    assert_eq!(TestBitEnum::from_bit(1 << 2), Some(TestBitEnum::Baz));
    assert_eq!(TestBitEnum::from_bit(1 << 3), None);
    assert_eq!(TestBitEnum::from_bit((1 << 0) + (1 << 1)), None);
}

#[test]
#[should_panic]
fn test_from_bit_unchecked() {
    TestBitEnum::from_bit_unchecked(1 << 3);
}

#[test]
fn test_bits_struct() {
    let mask = TestBitEnum::Foo + TestBitEnum::Bar;

    assert_eq!(
        TestBitEnum::all_bits(),
        Bits((1 << 0) + (1 << 1) + (1 << 2))
    );
    assert_eq!(mask, Bits((1 << 0) + (1 << 1)));
    assert_eq!(mask + TestBitEnum::Baz, TestBitEnum::all_bits());
    assert_eq!(TestBitEnum::all_bits(), TestBitEnum::try_iter().sum());
    assert_eq!(TestBitEnum::all_bits(), TestBitEnum::try_iter().collect());
    assert_eq!(
        mask,
        [&TestBitEnum::Foo, &TestBitEnum::Bar].into_iter().sum()
    );
    assert_eq!(
        mask,
        [&TestBitEnum::Foo, &TestBitEnum::Bar].into_iter().collect()
    );
    assert_eq!(
        TestBitEnum::all_bits(),
        [1 << 0, 1 << 1, 1 << 2].into_iter().sum()
    );
    assert_eq!(
        TestBitEnum::all_bits(),
        [1 << 0, 1 << 1, 1 << 2].into_iter().collect()
    );
    assert_eq!(
        TestBitEnum::all_bits(),
        [1 << 0, 1 << 1, 1 << 2].iter().sum()
    );
    assert_eq!(
        TestBitEnum::all_bits(),
        [1 << 0, 1 << 1, 1 << 2].iter().collect()
    );
    assert_eq!(mask, mask + TestBitEnum::Foo);
    assert_eq!(mask, mask + (1 << 0));
    assert_eq!(
        mask,
        [TestBitEnum::Foo, TestBitEnum::Bar, TestBitEnum::Bar]
            .iter()
            .sum()
    );
    assert_eq!(mask, [1 << 0, 1 << 1, 1 << 1].iter().sum());

    assert_eq!(TestBitEnum::zero_bit(), Bits::zero());
    assert_eq!(mask - TestBitEnum::Foo, TestBitEnum::Bar.bit());
    assert_eq!(mask - TestBitEnum::Bar, TestBitEnum::Foo.bit());
    assert_eq!(mask - TestBitEnum::Baz, mask);
    assert_eq!(mask - (TestBitEnum::Foo + TestBitEnum::Bar), Bits::zero());
    assert_eq!(
        mask - (TestBitEnum::Foo + TestBitEnum::Bar + TestBitEnum::Baz),
        Bits::zero()
    );

    assert!(mask.has(TestBitEnum::Foo));
    assert!(mask.has(TestBitEnum::Foo) && mask.has(TestBitEnum::Bar));
    assert!(mask.has(TestBitEnum::Foo + TestBitEnum::Bar));
    assert!(mask.has(1 << 0));
    assert!(!mask.has(TestBitEnum::Baz));
    assert!(!mask.has(1 << 2));
    assert!(!mask.has(TestBitEnum::all_bits()));

    assert!(mask.has_all([TestBitEnum::Foo]));
    assert!(mask.has_all(&[TestBitEnum::Foo]));
    assert!(mask.has_all([TestBitEnum::Foo].iter()));
    assert!(mask.has_all([TestBitEnum::Foo].into_iter()));
    assert!(mask.has_all([TestBitEnum::Foo, TestBitEnum::Foo, TestBitEnum::Foo]));
    assert!(mask.has_all([TestBitEnum::Foo, TestBitEnum::Bar]));
    assert!(!mask.has_all([TestBitEnum::Foo, TestBitEnum::Bar, TestBitEnum::Baz]));

    let mut i = mask.into_iter::<TestBitEnum>();

    assert_eq!(i.next(), Some(TestBitEnum::Foo));
    assert_eq!(i.next(), Some(TestBitEnum::Bar));
    assert_eq!(i.next(), None);

    let mut i = mask.iter::<TestBitEnum>();

    assert_eq!(i.next(), Some(TestBitEnum::Foo));
    assert_eq!(i.next(), Some(TestBitEnum::Bar));
    assert_eq!(i.next(), None);
}

#[test]
fn test_alias() {
    assert_eq!(TestAlias::Foo.alias(), TestAlias::Foo.value());
    assert_eq!(TestAlias::Bar.alias(), TestAlias::Bar.value());
}
