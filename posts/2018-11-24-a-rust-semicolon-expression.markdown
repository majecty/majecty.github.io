---
title: Rust expression과 semicolon
author: 주형
tags: rust
---

Expression
-----------

Rust는 모든 요소가 값을 가지진 않지만 꽤 많은 문법 요소들이 값을 가집니다. 특히 if/else문, block, match구문이 값을 가지는 expression입니다. 여러 문법적 요소가 expression을 지원하면, 값의 계산한 결과값을 변수 선언에 넣을 수 있기 때문에 immutable 하게 값을 사용하기 좋습니다.

```rust
// if가 statement라면
let x = true;
let mut a: Option<i32> = None;
if x {
    a = Some(3);
} else {
    a = Some(4);
}

println!("a : {}", a.unwrap());
```

```rust
// if가 expression이라면
let x = true;
let a = if x {
    3
} else {
    4
};

println!("a : {}", a);
```

block구문이 값을 가지기 때문에 일시적으로 필요한 변수들을 블록으로 감싸기 좋습니다.

```rust
let area = {
    let pi = 3.14;
    let r = 2;
    2 * pi * r
};
```

match는 다른 언어에서 만나도 보통 expression이죠.

```rust
let my_option = Some(3);
let a = match my_option {
    Some(x) => x * 2,
    None => 0,
}
```

Semicolon
----------

C언어를 사용할 땐 모든 statement에 semicolon을 붙입니다. Rust는 함수나 블록의 마지막 값에 semicolon을 붙이지 않으면 자동으로 해당 값이 return됩니다.

```rust
fn foo() -> i32 {
    println!("Here is foo");
    3
}

fn bar() -> i32 {
    println!("Here is bar");
    return 3;
}
```

이를 좀 다르게 생각해보면 `;`이 일종의 operator라는 관점을 가져볼 수 있습니다. expressionA `;` expressionB 를 실행시키면 expressionA를 실행한 뒤 값을 버리고 expressionB의 결과값을 return시키는 operator인거죠.