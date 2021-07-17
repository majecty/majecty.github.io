---
title: build.gradle 이해하기 2 - Gradle task
author: 주형
tags: java, gradle, groovy, understanding-gradle
---

이 글에서는 Java 프로젝트를 빌드하기 위해 많이 사용하는 Gradle의 기본 개념을 익힌다.

## Task의 개념

Gradle에서 가장 기본이 되는 개념은 Task다

우리는 빌드 도구를 사용해서 다양한 작업들을 한다.
소스코드를 빌드하기, 테스트하기, 린트 돌리기, 빌드 결과물 지우기 등의 작업들은 물론
특정 테스트만 돌리거나, 특정 플랫폼을 위한 결과물 빌드, 배포 등 프로젝트마다 
다양한 작업들을 빌드 도구로 실행한다.

빌드도구는 이런 다양한 동작들을 지원하기 위해서 사용자가 쉽게 동작을 정의할 수 있게 한다.
Gradle에서는 이런 각각의 동작들을 Task라고 부른다.
유저는 쉽게 Task를 정의하고, 실행할 수 있다.

## Task 예시

Task 개념에 익숙해지기 위하여
Task를 직접 정의하고 실행해보자.
아무 build.gradle 파일에 아래 코드를 추가한 뒤 `./gradlew hello`로 실행할 수 있다.

```groovy
tasks.register('hello') {
    doLast {
        println 'Hello world!'
    }
}
```

build.gradle을 자주 봐왔더라도 예시 코드가 어색할 수 있다.
평소 프로젝트의 빌드 설정이 간단하면 직접 Task를 정의할 일이 없기 때문이다.
Gradle의 Java 플러그인이 유용한 Task들을 미리 정의해주기 때문에 보통은
이미 정의된 task를 수정만 하면 된다.

## 미리 정의되어있는 Task들

Gradle의 BasePlugin에는 build, assemble, check
등의 task들이 정의되어있다.  이들은 [Lifecycle
task](https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks)라고
부른다.  우리가 `./gradlew build`를 호출하면 실행되는 task가 바로 이 build task다.

build.gradle에 특별한 내용이 없어도 `./gradlew build`를 하면 build가 된다.
어떤 과정을 통해서 build가 동작하는 걸까.

일반적인 자바 프로젝트에서 사용하는 build.gradle파일을
보면 다음 코드조각처럼 java 플러그인을 사용한다.

```gradle
plugins {
    id 'java'
}
```

java 플러그인은 내부에서 BasePlugin을 로드한다.
BasePlugin은 아무것도 못하는 build task를 정의한다.
java 플러그인은 jar task를 정의한 뒤 build task가 jar task에 의존하게 만든다.[^assemble]
`./gradlew build`를 실행하면 build가 의존하는 jar task가 실행된다.

[^assemble]: 사실 조금 다르다. BasePlugin이 build와 assemble을 정의한다.
  build 태스크는 assemble에 의존한다.
  java 플러그인이 jar를 정의하고 assemble이 jar에 의존하게 만든다.

## Task 덮어쓰기

Task를 정의한 뒤 어디에서든 Task의 동작을 수정할 수 있다.

```groovy
tasks.register('hello') {
    doLast {
        println 'Hello last'
    }
}

tasks.named('hello') {
    doFirst {
        println 'Hello first'
    }
}
```

위 코드처럼 hello 태스크를 정의한 다음에 언제든지 hello 태스크를 수정할 수 있다.
더 간단하게는 아래 코드같은 문법을 사용할 수 있다.

```groovy
hello {
  doFirst {
    println "hello first 2"
  }
}
```

## 정리

이상으로 Gradle의 태스크에 대해 알아보았다.  Task의 동작을
이해하면 잘 모르고 사용하던 build, clean, test등의 명령어가
어떻게 동작하는지에 대한 감을 얻을 수 있다. 다음
글에서는 Gradle이 사용하는 Groovy의 특별한 문법에 대해
이해한다.
