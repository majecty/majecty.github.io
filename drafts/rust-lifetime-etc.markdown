---
title: Null Exception이 안나는 Rust의 Lifetime
author: 주형
tags: rust
---

러스트는 lifetime을 사용하여 메모리를 관리한다.
라이프타임을 사용하여 Runtime에 잘못된 메모리에 접근하는 문제를 막을 수 있다.

상황을 분석하기 위하여 Null exception이 나는 상화을 몇 가지 정리하자.

1. 초기화되지 않은 메모리에 접근
1. 해제한 메모리에 접근
1. 

러스트는 가비지 컬렉팅 없이도 메모리를 안전하게 접근하게 해주는 언어이다.

뮤터블 레퍼런스와 이뮤터블 레퍼런스

레퍼런스
무브

길을 헤맸다.

Rust의 라이프타임 관리가 해주는 게 뭐지?

GC가 없지만 메모리 관리가 잘 되는 세상

기본적으로 레퍼런스 카운팅을 하는 언어가 있고,
Rust는 move?

항상 복사해서 가면 어떨까
항상 Reference를 늘리면 어떨까
바로우

Reference counting을 늘리지 않고도 혹은 복사하지 않고도 안전하게 사용하는 법
바로우
immutable borrow로 가지면 누군가 수정하지 안는 다는 것이 보장됨
mutable borrow로 가지면 나 혼자만 수정할 수 있다는 것이 보장됨
borrow의 사용 범위가 항상 값의 범위보다 작다는 것이 보장됨


C++의 레퍼른스도 소유가 아닌 레퍼런스만 하는 것.

소유와 바로우를 완전히 나눔
빌린 것은 소유할 수 없음

무엇을 한 걸까
GC없는 세상을 만든 것

Rust는 GC대신 lifetime과 Reference counting을 사용합니다. 
immutable과 reference

Rust의 Borrow는 재밌습니다. 소유권과 빌리는 상황을 명확히 구분합니다.
소유권은 RAII와 관련 있습니다. 

값을 소유

변수로 값을 소유

struct에 값을 소유

Box로 레퍼런스 값을 소유

Rc로 소유를 나눠가지기

Cell로 immutable인척 하는 mutable 만들기

소유와 빌림
-------

Rust는 값을 소유하거나 빌리는 두 가지로 구분한다. 가지고 있는 값은 다른 코드에게 값을 빌려줄 수 있다. 빌리는 건 값이 살아있다는 보장이 있을 때만 빌릴 수 있다. 가지고 있는 값을 버리면 버려진 값을 Drop함수가 호출되고, 버린 값을 누군가 빌리지 않았다는 사실이 컴파일 타임에 검증된다.

```rust
let a = 3;
let b = &a; // b는 a의 값을 빌렸다.
```

읽기 레퍼런스와 쓰기 레퍼런스
-----
Rust는 immutable과 mutable을 항상 구분한다. 한 값은 여러 immutable 레퍼런스를 가지거나 하나의 mutable reference를 가질 수 있다. 이는 RWlock의 규칙과 비슷하다.

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

값의 이동
------

빌리기 체계에서 매력적인 부분 중 하나가 값의 이동이다. 한 번 내가 변수에 할당했다고 해서 영원히 그 변수에 갖혀있지 않다. 언제든지 소유권을 이동시킬 수 있다.

```rust
let a = 3;
let b = a; // a의 값이 b로 이동되었다.
```

값이 이동은 struct안의 필드일 때 더욱 기묘하다. 필드에 있는 값을 밖으로 빼내려면, replace나 swap을 쓰거나, 스트럭트를 파괴할 때만 가능하다.

```rust
struct Me {
    key: Option<Key>,
}

impl Me {
    fn borrow_key(&self) -> &Key {
        self.key
    }

    fn steal_key(&mut self) -> Key {
        std::mem::replace(self,key, None)
    }

    fn kill_and_steal_key(self) -> Key {
        self.key
    }
}
```