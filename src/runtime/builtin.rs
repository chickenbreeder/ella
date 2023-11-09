pub(super) mod native {
    use crate::runtime::value::Value;

    pub fn print(v: Value) -> Value {
        println!("{v:?}");
        Value::None
    }
}
