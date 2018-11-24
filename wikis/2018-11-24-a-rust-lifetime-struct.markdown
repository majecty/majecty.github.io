---
title: Rust 빌린 값을 struct에 가지고 있기
author: 주형
tags: rust, rust-lifetime, rust-borrow
---

Rust에선 빌린 값들만 들고 있는 struct를 만드는 것도 가능하다.

아래 코드의 Foo의 lifetime은 a와 b의 lifetime과 같다
```rust
struct Foo<'a> {
    a: &'a str,
    b: &'b str,
}
```

레퍼런스는 값을 빌리는 방법이다. 빌린 값을 가지고 있는 struct는 빌린 값보다 오래 살 수 없다. 처음 코드를 짠다면 struct는 자신의 field를 항상 소유하고 있다고 가정하기 쉽다. 하지만 값을 소유하지 않고 빌려서 잠시 들고있는 struct들도 존재하고 이들도 꽤 쓸모가 있다.

값을 소유하는 예시
-----------------

struct가 값을 소유하는 상황은 매우 익숙하다.

아래의 `Person` 구조체는 이름과 주소를 소유하고 있다. `name`필드와 `address`필드는 `Person` 구조체가 파괴될 때 같이 파괴된다.

```rust
struct Person {
  name: String,
  address: String,
}
```

값을 빌려쓰는 예시
-----------------

계산기처럼 값을 빌려와서 다른 값을 생성하고 사라지는 구조체는 레퍼런스를 값으로 들고있는 게 자연스럽다.

```rust
struct StatisticCalculator<'a> {
    data: &'a [i32]
}

impl<'a> StatisticCalculator {
    fn get_max(&self) -> i32 { ... }
    fn get_min(&self) -> i32 { ... }
    fn get_average(&self) -> i32 { ... }
}
```