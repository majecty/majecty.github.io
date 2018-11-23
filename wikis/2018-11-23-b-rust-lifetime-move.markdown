---
title: Rust lifetme 옮기기
author: 주형
tags: rust, rust-lifetime
---

값의 이동
---------

러스트 라이프 타임에서 흥미로운 부분 중 하나가 값의 이동이다. 한 번 내가 변수에 할당했다고 해서 영원히 그 변수에 갖혀있지 않다. 언제든지 소유권을 이동시킬 수 있다.

```rust
let a = 3;
let b = a; // a의 값이 b로 이동되었다. 이 이후 a의 값은 쓰지 못한다.
```

이동과 스코프
------------

변수의 값은 일반적으로 스코프 안에서는 언제든지 접근 가능하다. 하지만 러스트에서는 스코프가 아직 끝나지 않더라도 옮겨진 값은 그 뒤로 사용할 수 없다. 스코프를 사용하는 다른 언어들은 값이 원치 않을 때 접근하는 걸 막으려면 블록을 하나 더 늘려야하기 때문에 코드가 더 복잡해지지만, 러스트에서는 컴파일러가 잘못된 사용을 막아준다.

```rust
let a = 3;
let b = a;
// 이 이후로 a를 사용하는 코드는 컴파일 에러이다.
```

struct 안의 값의 이동
---------------------

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

std::mem::{replace, swap}
-------------------------

Rust의 값의 이동은 함수 인자로 사용, 함수에서 리턴, 다른 변수에 대입에 의해서 일어난다. 하지만 이들만으론 struct 안에 선언되어있는 field의 값을 이동시킬 수 없다. 따라서 필드의 값을 이동시키는 별도의 방법이 필요하다. Rust는 std::mem::replaace함수와 std::mem::swap함수를 제공하여 이 문제를 해결한다.

replace는 mutable 레퍼런스를 가지고 있는 값에 다른 값을 집어넣고, 원래 있는 값을 밖으로 빼낸다. 이 때 값이 Clone되지 않고 Move된다.
```rust
pub fn replace<T>(dest: &mut T, src: T) -> T
```

```rust
struct MyTuple {
    pub a: String,
    pub b: String,
    pub c: String,
}

fn test() {
    let mut t = MyTuple { a: "a".to_string(), b: "b".to_string(), c: "c".to_string() };
    let b = std::mem::replace(&mut t.b, "xx".to_string()); // t안에 있단 b를 빼내고 대신 xx를 집어넣는다.
}
```

swap은 두 개의 mutable 레퍼런스가 있을 때, 그 두 값을 바꿔치기한다. replace도 swap을 통해 구현할 수 있다.

```rust
pub fn swap<T>(x: &mut T, y: &mut T)
```