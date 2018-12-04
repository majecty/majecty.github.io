---
title: Rust의 Trait
author: 주형
tags: rust, trait
---

필요성
------

코드의 재사용을 위해서 여러 타입을 받을 수 있는 코드를 작성해야한다. Trait을 정의하면 내가 사용하고자 하는 타입이 특정한 타입들을 구현함을 강제할 수 있다.

사용
----

### 정의

Trait은 함수들을 나열하여 정의한다. 구현이 없이 명세만 있을 수도 있으며, 기본 구현을 제공할 수 있다.

```rust
trait Movable {
    fn move(&mut self, point: Point);
    fn move_go_two_place(&mut self, point1: Point, point2: Point) {
        self.move(point1);
        self.move(point2);
    }
}
```

### 값을 받기

Generic과 함께 쓰거나, Trait Object로 사용할 수 있다. 하지만 Generic 문법과 함께 사용하는 것을 추천한다.[1][link-1]

#### Generic과 사용

```rust
fn move_generic<T: Movable>(a: &mut T) {
    a.move(Point::new(0, 0));
}
```

#### Trait object 사용

```ㅐㄷㄴㅓ
ㅏㅅ=========


참고자료
--------

[link-1]: /posts/2018-12-01-a-rust-trait-generic-trait-object.html
1. <span id="link-1">[Rust trait 사용하기 Generic vs Trait Object][link-1]</span>