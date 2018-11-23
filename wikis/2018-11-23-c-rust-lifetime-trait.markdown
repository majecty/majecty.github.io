---
title: Rust lifetime과 trait
author: 주형
tags: rust, rust-lifetime, rust-trait
---

러스트에서 값으로 존재하는 것은 크기가 알려진 타입들만 가능하다. struct, enum, primitive 등의 타입들은 스택에서의 크기가 정해져있다. 반면 trait은 타입의 조건만을 명시하므로 trait의 타입의 값의 크기를 실행하기 전까지 알 수 없다. 따라서 trait타입의 값은 rust에 존재하지 않는다.

trait을 다루려면 레퍼런스를 사용하거나 generic을 사용하여야한다.

```rust
// 컴파일 되지 않음
let s: String = "This is a string".to_string();
let x: Display = s;
```

```rust
// 잘 동작함
let s : String = "This is a string".to_string();
let x: &Display = s;
```