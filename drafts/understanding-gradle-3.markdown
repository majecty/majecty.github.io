---
title: build.gradle 이해하기 3 - Groovy 문법 이해
author: 주형
tags: java, gradle, groovy, understanding-gradle
---

* Groovy에서 괄호 생략하기
* Groovy의 람다?
* Groovy에서 람다에 글로벌 변수 주기.

build.gradle의 동작을 이해하려면 Groovy 문법을 알면 좋다.
Groovy는 다른 언어에서 특이해 보이는 문법을 몇 가지 쓰기 때문에 문법을 모르면 헷갈릴 수 있다.
Groovy 문법을 봤을 때 Ruby와 많이 비슷한 느낌이 들었다.

## 괄호의 생략

Groovy는 함수를 호출할 때 괄호를 생략할 수 있다. 

## `{}`로 사용하는 클로져

클로져가 블록처럼 생겼다.
클로져를 마지막 인자로 넣을 때 특별하게 사용할 수 있다.
클로져에 delegatee를 설정하면 해당 인자 . 이 생략된 것 처럼 동작한다.

## 예시 읽기

괄호 생략 예시
`{}` 클로져 예시
`delegatee` 예시
문서로 찾을 수 있는 delegatee
