---
title: Rust lifetme 소유와 빌림
author: 주형
tags: rust, rust-lifetime
---

소유와 빌림
-------

Rust는 값을 소유하거나 빌리는 두 가지로 구분한다. 가지고 있는 값은 다른 코드에게 값을 빌려줄 수 있다. 빌리는 건 값이 살아있다는 보장이 있을 때만 빌릴 수 있다. 가지고 있는 값을 버리면 버려진 값을 Drop함수가 호출되고, 버린 값을 누군가 빌리지 않았다는 사실이 컴파일 타임에 검증된다.

```rust
let a = 3;
let b = &a; // b는 a의 값을 빌렸다.
```

읽기 레퍼런스와 쓰기 레퍼런스
-----
Rust는 immutable과 mutable을 항상 구분한다. 한 값은 여러 immutable 레퍼런스를 가지거나 하나의 mutable reference를 가질 수 있다. 이는 RwLock의 규칙과 비슷하다.

```rust
// 가능
let a = 3;
let b = &a;
let c = &a;
```

```rust
// 불가능
let mut a = 3;
let b = &mut a;
let c = &mut a;
```
