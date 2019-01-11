#[macro_use]
extern crate serde_derive;

use serde::{self, de, Deserialize};
use serde_fallback::serde_fallback;

#[serde_fallback]
#[derive(Deserialize)]
struct Testing {
    #[serde_fallback(default="test", deserialize_with="deserialize_test")]
    val1: u16,
    #[serde_fallback(default="test")]
    val2: u16,
    val3: Vec<u32>,
}

fn main() {
    let input = r#"
        val1: "0a"
    "#;
    let test: Testing = serde_yaml::from_str(input).unwrap();
    println!("{} - {} - {:?}", test.val1, test.val2, test.val3);
}

fn test() -> u16 {
    77
}

fn deserialize_test<'a, D>(deserializer: D) -> ::std::result::Result<u16, D::Error>
    where D: de::Deserializer<'a>
{
    match u16::deserialize(deserializer) {
        Ok(val) => Ok(val),
        Err(err) => {
            println!("Problem with config: {}; using default value", err);
            Ok(97)
        },
    }
}
