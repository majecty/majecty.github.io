---
title: Rust trait 사용하기 Generic vs Trait Object
author: 주형
tags: rust, trait, trait-object, generic
---

## 한 줄 요약

Trait은 Generic과 같이 쓰자.

## Trait의 구현과 사용

Rust에서 여러 타입의 코드에 대해서 중복되는 기능이 필요한 경우 Trait 문법을 사용하여 중복을 제거할 수 있다. Trait은 함수들만을 정의하는 타입으로, 다른 Trait이나 Struct, Reference 타입들이 Trait을 구현할 수 있다.

Trait 의 정의는 trait과 연결된 함수들을 정의하며, Trait을 구현하는 struct들은 해당 함수들을 구현해야한다. Rust generic에 Trait바운드를 걸어서 Trait을 구현한 타입들만을 위한 코드를 작성하여 코드의 중복을 줄일 수 있다.

**Trait의 impl**
```rust
    struct Player {}
    
    struct Enemy {}
    
    trait Movable {
    	fn move(&self, target: Position);
    }
    
    impl Movable for Player {
      fn move(&self, target: Position) {
    		// ...
    	}
    }
    
    impl Movable for Enemy {
    	fn move(&self, target: Position) {
    		// ….
    	}
    }
```

**Trait의 사용.**

Trait을 함수의 인자로 사용할 때 타입 위치에 `impl trait-name" ` 방식으로 사용할 수 있다.

```rust
    fn move_to_goal(object: &impl Movable)
    {
      object.move(goal);
    }
```

**generic 사용**

impl trait-name 구문은 generic을 사용하는 구문의 신택틱 슈가이다. 위의 코드와 아래 코드는 같다.

```rust
    fn move_to_goal<T: Movable>(object: &T) {
    	object.move(goal);
    }
```

## Trait 주의점

Trait을 struct 타입 대신 사용할 수 없다. Trait 타입의 변수를 선언하거나 trait 타입의 필드를 선언할 수 없다. Rust는 기본적으로 값이 옮겨지거나 복사되는 방식으로 동작하기 때문에 변수를 선언하거나 필드로 사용할 때 값의 크기가 알려진 타입만을 쓸 수 있다. Trait은 어떤 타입의 struct가 올 지 모르기 때문에 값을 미리 정할 수 없으므로 변수나 필드로 쓸 수 없다. 대신 특정 trait을 구현한 generic 타입을 필드로 쓸 수 있다.

## Trait object

Rust는 값을 바탕으로 동작하며 어떤 값 a의 함수 foo를 호출하는 코드가 있다면 실제 어떤 코드가 호출되는 지는 컴파일타임에 확정된다. 간혹 특별한 경우에 객체지향 언어처럼 실행시점에 어떤 함수를 호출할지 결정하는 상황이 필요할 수 있다. 그 경우 trait object 문법을 사용하여 구현할 수 있다.

하지만 Trait object 문법은 러스트가 권장하는 문법이 아니다.  Rust는 스택에 값을 생성하는 것을 권장하는데, Trait object는 Heap에 값을 생성해야한다. 또한 static dispatching을 이용한 zero cost abstraction이 Rust의 장점이지만, Trait object는 dynamic dispatching이기 때문에 함수 호출에 오버헤드가 있다.

Trait object는 특수한 문법으로 다른 러스트 코드들과 같이 쓰기 힘들다. generic으로 가져온 값은 trait object로 바꾸는 것도 가능하고, 다른 러스트 타입을 쓰는 것과 똑같이 쓸 수 있다. 하지만 Trait object로 가지고 있는 값은 generic을 받는 함수에 넘길 수없다.

```rust
    // Generic을 사용한 버전
    fn move_to_goal<T:Movable>(object: &T) {
      object.move(goal);
    }
    
    // Trait object를 사용한 버전
    fn move_to_goal(object: &Movable) {
      object.move(goal);
    }
```

## 결론

러스트의 Trait은 코드 재활용을 위해서 꼭 필요한 기능이다. Generic 문법을 사용하여 특정 Trait을 구현한 값을 가져와 동작하는 코드를 작성할 수 있다. 특수한 상황에서 Trait을 구현한 임의의 타입을 받아야하는 경우 Trait object의 문법을 사용할 수 있다. 하지만 Trait object는 특수한 문법으로 다른 러스트 코드들과 같이 쓰기 힘들기 때문에 될 수 있으면 사용을 하지 않는 것이 좋다.