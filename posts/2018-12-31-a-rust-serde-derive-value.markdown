---
title: Rust serde derive와 Value
author: 주형
tags: rust, serde, JSON
---

## tldr;

형식이 정해진 JSON 데이터는 struct나 enum으로 serialize/deserialize하고, 형식이 자유로운 데이터는 Value 타입을 사용하자.

## Derive 방식

Rust에서 JSON 객체를 파싱할 때 가장 기본적인 방법은 struct로 변환하는 것이다. serde의 derive는 대부분의 타입에 대해서 serialize/deserialize 코드를 자동으로 생성한다.

struct 타입의 값을 JSON으로 serialize/deserialize 하려면 JSON 데이터의 형태가 고정되어있어야한다. 

```rust
// struct의 serialize / deserialize 예시
#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct Person {
  name: String,
  age: i32,
}

fn main() {
  let person: Person = Person {
    name: "jh".to_string(),
    age: 3,
  };

  let serialized = serde_json::to_string(&person).unwrap();
  println!("Serialized {}", serialized);
  // Serialized {"name":"jh","age":3}

  let deserialized: Person = serde_json::from_str(&serialized).unwrap();
  println!("Deserialized {:?}", deserialized);
  // Deserialized Person { name: "jh", age: 3 }
}
```

위 코드는 Person struct를 serialize/deserialize하는 예시이다. 위와 같이 형식이 정해져있는 JSON 데이터는 쉽게 변환이 가능하다.

## 형식이 정해져있지 않은 데이터의 처리

하지만 모든 데이터의 형식이 정해져있는 것은 아니다. 유저가 직접 JSON을 생성해 넣는다면 가능한 임의의 값이 키값으로 생성될 수 있다. serde json의 Value타입을 사용하면 이런 타입들을 쉽게 serialize/deserialize할 수 있다.

```rust
// value의 serialize / deserialize 예시
extern crate serde;
extern crate serde_json;

use serde_json::Value;

fn main() {
  let data = r#"{
                  "name": "jh",
                  "age": 73
              }"#;
  let v: Value = serde_json::from_str(data).unwrap();
  println!("name is {}, age is {}", v["name"], v["age"]);
}
```

위 예시는 JSON string을 Value로 읽어오는 예시이다. 

## 두 방식의 혼합

JSON 데이터가 일부분은 형식이 정해져있고 일부분은 형식이 안정해져있을 수 이다. 이 경우 derive방식과 Value방식을 혼합하여 사용할 수 있다. Serialize 트레잇을 정의한 타입은 serde_json::to_value를 사용하여 해당 타입을 Value타입으로 변환할 수 있다. Deserialize 트레엣을 정의한 타입은 serde_json::from_value를 사용하여 Value타입에서 원하는 타입으로 변환할 수 있다.