use enumeration::prelude::*;

enumerate!(Test(u8; i32 = 99)
    X = 111
    Y
    Z = 777
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
