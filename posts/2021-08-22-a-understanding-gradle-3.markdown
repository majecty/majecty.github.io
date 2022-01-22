---
title: build.gradle 이해하기 3 - Groovy 문법 이해
author: 주형
tags: java, gradle, groovy, understanding-gradle
---

* [build.gradle 이해하기 1 - Gradle은 무엇인가](./2021-07-17-a-understanding-gradle-1.html)
* [build.gradle 이해하기 2 - Gradle task](./2021-07-17-b-understanding-gradle-2.html)
* [build.gradle 이해하기 3 - Groovy 문법 이해](./2021-08-22-a-understanding-gradle-3.html)

build.gradle에서 쓰이는 Groovy 문법을 이해하면 build.gradle을 더 깊게 이해할 수 있다.
프로그래밍 언어들 문법이 거기서 거기긴 하지만 Groovy는 조금 특이한 문법을 가지고 있다.
알면 간단하지만 모르면 헷갈리기 때문에 한 번 읽고 이해해두면 이해해두면 좋다.

## 괄호의 생략

Groovy는 함수를 호출할 때 괄호를 생략할 수 있다. 

```groovy
println(3)
println 3
println("x")
println "x"
```

## `{}`로 사용하는 클로져

상당히 재밌는 문법이 있다. 클로져의 문법이 중괄호다.
이를 활용하면 직접 for문 이나 if문과 비슷한 문법을 직접 만들 수 있다.

클로져에 인자가 하나인 경우 `it`이란 이름에 자동으로 바인딩된다.

```groovy
[1,2,3].each { println it }
```

### 클로져와 델리게이션

특히나 신기했던 문법이 클로져의 델리게이션이다.
클로져에 객체 하나를 delegate로 설정하면, 클로져 안의 코드가 `delegate.`이 생략된
것 처럼 동작한다.

```groovy
def p = new Person(name: "sue")
def upper = { name.toUpperCase() }
upper.delegate = p
println upper() // sue가 출력된다.

def p2 = new Person(name: "sim")
upper.delegate = p2
println upper();// sim이 출력된다.
```

위 코드에서 `upper` 클로져 안의 코드가 정의되지 않은 이름 `name`을 사용한다.
`upper` 클로져를 실행하기 전에 `delegate`로 `name` 필드를 가진 `Person` 클래스의 객체를 설정한다.
이렇게 하면 `upper`클로져를 실행할 때 `delegate.name.toUpperCase()`가 호출된다.
build.gradle에서 엄청나게 자주 사용되는 문법이다.


## 예시 읽기

build.gradle에서는 `<설정이름> <설정값>` 이렇게 뛰어쓰기로 구분된 단어의 나열을
자주 사용한다. 이는 `<설정이름>(<설정값>)`이랑 같다.
`<설정이름>` 이라는 함수에 `<설정값>`을 인자로 주어 실행한 것이다.

build.gradle에서 모든 `{}`는 클로져라고 보면 된다.
`{}` 안에서 사용되는 함수들은 클로져에 delegate된 객체의 함수다.
아래 예시를 보면서 이해해보자.

```groovy
dependencies {
    testImplementation 'org.junit.jupiter:junit-jupiter:5.7.2' 
    implementation 'com.google.guava:guava:30.1.1-jre' 
}

```

여기서 `dependencies` 는 함수로 클로져를 하나 인자로 받는다.
`dependencies`는 인자로 받은 클로져에 `DependencyHandler` 객체를 `delegate`한다.
즉 `testImplementation`이나 `implementation`은 `DependencyHandler` 타입에 정의된 메쏘드다.
[dependencies의 문서](https://docs.gradle.org/current/dsl/org.gradle.api.Project.html#org.gradle.api.Project:dependencies(groovy.lang.Closure))를 보면 클로져의 delegate로 무엇이 설정되는지 잘 정리되어 있다. 


