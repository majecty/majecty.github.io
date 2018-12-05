---
title: Rust trait 더하기 lifetime
author: 주형
tags: rust, trait, trait-object, generic
---

한줄 요약
----------

레퍼런스 타입이 Trait을 구현할 때 레퍼런스 타입의 lifetime을 Trait 타입 + 'lifetime 형식으로 표현한다.

추가 설명
--------

struct나 enum타입 이외에 이들의 reference타입들도 Rust의 Trait을 구현할 수 있다. 따라서 Trait타입과 관련된 변수를 사용할 때 Trait타입의 lifetime도 명시해주어야한다.

T + 'static
------------

```rust
fn foo<T: MyTrait + 'static>(T: arg) {
    arg.my_trait_func();
}
```

위와 같은 함수가 있다면 두 가지 경우 중 하나를 의미한다. arg가 reference타입이고, arg의 lifetime이 'static을 의미하거나, 혹은, arg가 struct나 enum, primitive타입을 의미한다.
