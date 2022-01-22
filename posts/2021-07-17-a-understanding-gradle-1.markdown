---
title: build.gradle 이해하기 1 - Gradle은 무엇인가
author: 주형
tags: java, gradle, groovy, understanding-gradle
---

* [build.gradle 이해하기 1 - Gradle은 무엇인가](./2021-07-17-a-understanding-gradle-1.html)
* [build.gradle 이해하기 2 - Gradle task](./2021-07-17-b-understanding-gradle-2.html)
* [build.gradle 이해하기 3 - Groovy 문법 이해](./2021-08-22-a-understanding-gradle-3.html)

build.gradle은 매우 쉬워보이고, 간단한 일을 간단하게 할
수 있다.  라이브러리 추가도 해보고, 삭제도 해 본 뒤,
Gradle 별 거 아니네 생각하기 쉽다.  하지만 build.gradle이 한
번 꼬이면 막막하다.  구성 원리를 이해해야 고칠 수 있기
때문이다.  앞으로 build.gradle을 이해하기 위한 **최소한의**
지식을 쌓아보자.

Gradle은 Java 프로젝트들이 많이 사용하는 빌드 도구다.
Gradle은 프로젝트의 빌드에 관한 대부분의 설정을 build.gradle
파일에 관리한다.  build.gradle 파일은 간결하고 익숙한
문법을 사용한다.  프로젝트가 사용하는 라이브러리를
추가, 삭제, 버전업 등과 같은 작업은 따로 문서를 보지
않아도 쉽게 할 수 있다.

Gradle이 간결한 문법을 사용할 수 있는 이유는 Gradle이
사용하는 Groovy라는 언어의 특징 때문이다.  Groovy는 유연한
문법을 제공한다. 간결해 보이는 Gradle의 설정파일은
파고들어가 보면 생각보다 복잡한 Groovy의 문법을 사용해
구현되어 있다.

이렇게 언어의 자유로운 문법을 활용하여 필요한 영역의
문제를 해결하는 간결한 언어를 새로 만드는 방법을
DSL(Domain Specific Language)이라고 부른다.  Groovy는 DSL에 특화된
언어고 Groovy는 이 DSL을 잘 구현한 예시다.

Gradle을 쓰면서 아쉬운 점이 있었다. 간단한 기능을
사용하는 건 쉽지만, 고급 기능을 사용하는 것이 무척
어렵다는 점이다.  그래서 잘 쓰던 스크립트가 문제가
생기면 원인 파악이 힘들어진다.  자연스럽게 구글에 에러
메시지를 검색하고, 무엇이 문제인지도 모른채 복붙만
하면서 시간을 날리게 된다.  엔지니어로서 참으로 아쉬운
상황이 된다.

나는 이 문제가 Gradle의 욕심과 디자인 실수의 결합이
원인이라고 생각한다.  Gradle 홈페이지를 들어가면
일단 당황하게 되는데, 그 이유는 Gradle이 만능 도구이기
때문이다.  나는 분명 Java프로젝트를 관리하기 위해 Gradle을
사용하는데 Gradle은 만능 빌드 도구이다.  결국 여기저기
뒤져가면서 내가 원하는 기능이 Gradle내장 기능인지 Java
플러그인의 기능인지 찾아야한다.  이 과정에서 Gradle의
구성 원리를 이해해야하고, 그럼 Gradle을 처음부터 공부해야
한다.  분명 나는 잘 쓰던 스크립트의 사소한 문제를
해결하러 왔는데, 갑작스럽게 공부할 양이 산더미가 된다.

만약 Gradle이 딱 Java용 기능만 제공하고, 그 기능들이
DSL의 문법과 잘 어우러졌다면 DSL의 문법을 익히는
것만으로도 충분히 모든 문제를 해결할 수 있을 것이다.
Gradle은 빌드만을 위한 간결한 DSL을 만드는 것에 실패했다.
아쉽게도 우리는 필요한 만큼 Gradle을 공부해서 이해해야
한다.

다음 글부터 build.gradle을 이해하기위한 지식을 쌓아보자.
