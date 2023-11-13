pub(super) mod native {
    use crate::runtime::value::Value;

    pub fn print(v: Value) -> Value {
        println!("{v:?}");
        Value::None
    }

    pub fn assert_eq(v: Value) -> Value {
        if let Value::List(values) = &v {
            assert_eq!(2, values.len());
            assert_eq!(values[0], values[1]);
            return Value::None;
        }
        panic!("Function only accepts lists with 2 elements: {v:?}");
    }
}
