---
title: Java의 마법 - Proxy
author: 주형
tags: java, proxy
---

이전 글에서 자바의 보수적인 측면을 이야기했다. 자바는 한 번 노출시킨
인터페이스를 최대한 깨트려 먹지 않으려는 문화를 가지고 있다. 이를
뒤집어 생각하면 인터페이스만 깨트리지 않으면 구현이 얼마든지 바뀔 수
있다고 이해할 수 있다. 규칙이 있기 때문에 오히려 창의적이다.

### Spring Data JPA 쿼리가 리턴하는 값

자바에 익숙지 않을 때 라이브러리에서 클래스에 어노테이션을 다는 게
이해가 되지 않았다. 자바의 어노테이션은 단순히 추가 정보를 추가하는
것이고, 어디선가 그 값을 읽어서 쓰게 된다.

어노테이션이 달린 클래스들의 동작은 쉽게 이해하기 어려웠다. 분명 내가
만든 클래스를 내가 사용하는데 내가 작성한 코드대로 동작하지 않는다.
내가 알던 그 클래스가 아니었다. 나는 동작이 궁금할 때 코드를 보고
완전히 이해하는 걸 좋아한다. 처음에는 내가 짠 자바 코드가 어떻게
동작하는지 코드를 읽고 이해하고 싶었다. 하지만 쉽지 않았다.
라이브러리의 함수가 내가 정의한 타입의 값을 리턴하는데 해당 값의
동작은 내가 작성한 코드와 달랐다.

 회사 일을 하면서 디비에서 정보를 조회하기 위해서 Spring Data JPA를
사용했다. 이 때 쿼리의 결과로 내가 정의한 클래스가 리턴된다..

[JPA](https://www.oracle.com/technical-resources/articles/java/jpa.html)는
자바에서 객체지향 코드로 디비를 접근하는 표준이다. 하나의
클래스의 인스턴스가 디비의 하나의 row를 표현한다. 이 row에 필드로 다른
디비의 row를 연결할 수 있다.

```java
@Entity
class Car {
  @Id
  String serialNumber;
  @OneToMany(fetch = FetchType.LAZY)
  List<Wheel> wheels;
}
```

여기서 Car 클래스는 Car 테이블에 대응되고, Car클래스의 인스턴스 하나
하나가 DB의 row를 의미한다. 여기서 wheels는 lazy loading된다. lazy
loading의 동작에 대해 알아보자.

```java
  List<Wheel> myWheels = car.getWheels();
```

이렇게 `getWheels`로 `List<Wheel>` 값을 가져와 `myWheels` 변수를
선언했다고 치자. 이 `myWheels`는 아직 진짜 `wheel`값을 가지고 있지
않다. 껍데기만 가지고 있다. `myFriends.size()`를 호출하면 그제서야
디비로 쿼리를 보내, 값을 가져온다. 내가 작성한 `Car`에는 도대체 이런
코드가 없는데 어떻게 이런 일이 일어난 걸까.

알고보니 DB library(Hibernate)가 디비 쿼리의 결과물로 내가 작성한
`Car` 클래스를 상속한 클래스를 리턴하고 있었다! 나도 모르게 다른
구현체를 쓰고 있었고, 그 구현체의 코드는 찾아볼 수 없다. 왜냐면
런타임에 생성된 클래스이기 때문이다.[^proxy-hibernate]

[^proxy-hibernate]: Hibernate의 유저 가이드에서 [Entity types 섹션](https://docs.jboss.org/hibernate/orm/5.4/userguide/html_single/Hibernate_User_Guide.html#entity)
    을 보면 Proxy에 대한 내용을 찾아볼 수 있다.

마법같은 일이다. 런타임에 내 클래스를 상속해서 proxy 객체를 만든다니.
런타임에 생성된 클래스이기 때문에 당연히 해당 클래스의 소스코드를
찾아볼 수 없었다. 난 Hibernate가 내 클래스를 상속해서 어떤 클래스를
만들어 쓰는지 궁금하다. 이걸 알아내려면 Hibernate가 Proxy를 생성하는
코드를 읽어야 한다.

### 스프링의 DI

이와 비슷한 일이 Spring의 DI에서도 일어난다. Spring DI에서는 꽤 많은
일이 일어난다. 그 중 하나로 Spring Data JPA를 보자. Spring Data JPA를
쓸 때 실수하기 좋은 부분이 하나 있다. 바로 메쏘드가 디비 트랜잭션
안에서 실행되게 만드는 `@Transactional` annotation이다. 트랜잭션이
필요한 코드인 경우 메쏘드 위에 `@Transactional` 어노테이션을 붙이면
해당 메쏘드는 디비 트랜잭션 안에서 실행된다. 무척 편하다.

```java
class MyFantasticClass {
  void countUp10Times() {
    for (int i=0; i<10; i+=1) {
      countUp();
    }
  }

  @Transactional
  void countUp() {
    Counter counter = this.countRepository.getById(1);
    counter.countUp();
    this.countRepository.save(counter);
  }
}
```

이 코드의 `myFantasticClass.countUp()`을 호출하면 `countUp`은 디비
트랜잭션 안에서 실행된다. 하지만 `myFantasticClass.countUp10Times()`를
실행하면 디비 트랜잭션이 실행 안된다.[^understanding-spring-transaction]

[^understanding-spring-transaction]:
    Spring의 [Understanding the Spring Framework’s declarative transaction implementation](https://docs.spring.io/spring-framework/docs/4.2.x/spring-framework-reference/html/transaction.html#tx-decl-explained)
    에 무슨 일이 일어나는지 잘 설명되어 있다.

왜일까. 그 이유는 `@Transactional`가 사용되는 방법과 관련된어 있다.
`MyFantasticClass`에는 디비 트랜잭션을 실행하고 끄는 코드가 없다. 이
클래스를 `new MyFantasticClass()`로 생성하면 `@Transactional`이 아무
역할도 하지 않는다. `@Transactional`은 `MyFantasticClass`를 DI를
통해서 주입받을 때 효과를 발휘한다.

Spring DI는 `MyFantasticClass` 인스턴스를 받은 뒤 `MyFantasticClass`를
상속한 새로운 클래스로 해당 인스턴스를 감싸서 리턴한다.[^proxy] 따라서
디비 트랜잭션을 시작하고, 끝날 때 commit하는 코드는
`MyFantasticClass`를 상속한 클래스에 들어간다.

[^proxy]: JDK Dynamic Proxy 혹은 cglib을 사용해서 구현한다.
    [understanding aop proxies](https://docs.spring.io/spring-framework/docs/4.2.x/spring-framework-reference/html/aop.html#aop-understanding-aop-proxies)
    문서를 보면 더 잘 이해할수 있다.

```java
// 이런 식으로 구현된다고 이해할 수 있다.
class MyFantasticGeneratedSubClass extends MyFantasticClass {
  private MyFantasticClass inner;

  void countUp10Times() {
    inner.countUp10Times();
  }

  void countUp() {
    startTransaction();
    inner.countUp();
    commitTransaction();
  }
}

```

DI로 주입받은 `myFantastiClass`의 `.countUp()`을 호출하면 무슨 일이
일어날까. 당연히 함수의 앞뒤로 트랜잭션의 시작과 commit이 호출된다.
그렇다면 DI로 주입받은 `myFantasticClass`의 `countUp10Times`를
호출하면 무슨 일이 일어날까. 해당 함수는 `@Transactional`이 없기
때문에 앞뒤에 추가되는 코드가 없다. 그리고 그 안에서 `countUp`을
호출하면 원본 코드의 `countUp`이 실행되기 때문에 디비 트랜잭션이
실행되지 않는다![^self-invocation]

[^self-invocation]: Spring AOP에서는 이를 self-invocation이라고 부른다.
    `@Transactional`뿐만 아니라 Spring AOP를 쓰는 코드에서는 항상
    발생한다. AspectJ를 사용하면 이 문제를 회피할 수 있다.

### 클래스는 구현이 첨가된 인터페이스일 뿐

자바 코드를 작성하면서 "클래스 역시 구현이 조금 추가된 인터페이스"라고
느꼈다. 언제든지 상속을 통해 확장될 수 있다. 라이브러리가 어떤 일을
하는지 알고싶으면 문서를 찾아봐야한다. Spring Data JPA, Hibernate 모두
방대한 문서를 제공하고 그 안에 동작 방식이 설명되어 있다.

[이전 글 - Java는 보수적이야](./2022-01-23-b-java-conservative-patterns.html)

