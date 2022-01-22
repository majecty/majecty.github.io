---
title: Java에서는 라이브러리 쓰기가 무섭다
author: 주형
tags: java, dependency
---

새로운 언어 환경을 보다 보면 라이브러리가 어떻게 관리되는지 보는 게 재밌다. 내가
지금까지 많이 써보고, 라이브러리가 어떻게 동작하는지까지 알아본 언어(환경)은
Node.js와 Rust였다. 둘은 SemVer도 쓰고, 여러 버전의 라이브러리가 복사되어 사용된
다는 점에서 비슷한 점이 많았다. 여러 버전이 들어가다 보니 하나만 들어가 있다고
가정한 변수가 여러 개 생기는 문제가 생기기도 했었다. 이를 막으려는 장치를 넣은
라이브러리도 만났었는데 ([bitcore-lib](https://github.com/bitpay/bitcore/tree/v8.0.0/packages/bitcore-lib)),
툭하면 런타임 에러를 만들어서 고생했었다.

작년부터 자바를 쓰는 회사에 들어가면서 자바를 제대로 쓰기 시작했다. 자바는
Node.js, Rust와는 다른 전략을 쓰고 있었다.

### package name은 unique

package name이 유일해야한다는 점이 흥미로웠다. 1.3 버전의 my-fun-library와 3.9
버전의 my-fun-library가 모두 my.fun.library.Foo 클래스를 정의했다면 문제가
생긴다. 실행되는 자바 프로그램은 단 하나의 my.fun.library.Foo를 쓸 수 있기
때문이다. 두 클래스 모두를 쓸 방법은 없다.

이는 충격적이었다. 내가 한 번 라이브러리에서 클래스를 배포하면 나는 영원히 이전
클래스의 굴레에 갇히게 된다. 내가 1.3버전에서 my.fun.library.Foo 클래스를
배포했으면, 앞으로 영원히 모든 버전의 my.fun.library.Foo를 1.3 버전의
my.fun.library.Foo와 같이 쓰일 수 있게 만들어야 한다. 1.3에서 만든 constructor를
영원히 지원해야하고, 1.3 에서 공개한 public method는 지울 수 없다.

라이브러리 코드에 큰 변화를 가져오려면 완전히 새로운 package name 써야 한다.
Java의 유명한 테스트 프레임워크인 JUnit은 5버전에서는 클래스들이
`org.junit.jupiter` 혹은 `org.junit.platform`으로 시작하는 패키지 이름을
사용한다. JUnit 4버전은 `org.junit`을 사용한다. `jupiter`와 `platform`을 사용해
이전 클래스 들과 패키지 이름이 겹치지 않게 만들었다.

- [JUnit 5.8.2 doc](https://junit.org/junit5/docs/current/api/)을 보면
  `org.junit.jupiter` 혹은 `org.junit.platform`, `org.junit.vintage`로 시작하는
  것을 볼 수 있다.
- [JUnit 4.13.2 doc](https://junit.org/junit4/javadoc/latest/index.html)을 보면
  `org.junit`로 시작하는 클래스들을 볼 수 있다.

### 어떤 버전의 라이브러리를 사용할 것인가

나는 SemVer가 익숙하다. Node.js와 Rust가 기본적으로 SemVer를 쓰기 때문이다.
[SemVer](https://semver.org/)를 사용하면 한 프로그램이 사용하는 라이브러리의
버전을 범위로 지정할 수 있다. 한 프로그램이 라이브러리를 여럿 사용하다 보면 여러
라이브러리가 공통으로 사용하는 라이브러리들이 있다. 모든 라이브러리가 SemVer를
사용하면 공통으로 사용되는 라이브러리의 여러 버전 중 하나의 버전을 쉽게 고를 수
있다.

Java는 SemVer를 쓰지 않는다. 애초에 어떤 라이브러리를 가져와 쓸지에 대해서 Java
언어 표준이 없다. Java는 프로그램이 시작될 때 필요한 클래스들을 class path가 가
리키는 디렉토리에서 찾아서 로드한다. 내 코드와 라이브러리 코드의 구분도 없다.
빌드된 class 파일들만 있다.

언어의 표준이 없는 대신 Maven이나 Gradle과 같은 빌드 도구가 라이브러리를
관리한다. `A` 프로그램이 사용하는 라이브러리가 `B`고, `B` 라이브러리가 사용하는
라이브러리가 `C`가 있다고 하자. `A` 프로그램을 빌드하면 Maven 이나 Gradle이
`B`를 다운 받고 다시 `C` 라이브러리를 다운받는다.

좀 더 복잡한 예를 보자. `My App`이 `Lib A 1.2.0`과 `Lib T 1.3.4`를 사용한다.
`Lib A 1.2.0`은 `Lib X 1.0.1`을 사용한다, `Lib T 3.1.0`은 `Lib S 2.0.0`을
사용한다. `Lib S 2.0.0`은 `Lib X 1.4.0`을 사용한다.

![dependency graph](/images/java-dependencies.png)

자바는 한 라이브러리의 두 버전을 동시에 사용할 수 없으므로, 하나의 버전을 골라야
한다. Maven의 경우 root에 가까운 걸 선택하고, Gradle은 버전이 가장 높은 걸
선택한다. Maven은 `Lib X 1.0.1`을 선택하고, Gradle은 `Lib X 1.4.9`를 선택한다.

[Gradle의 Understanding dependency resolution](https://docs.gradle.org/current/userguide/dependency_resolution.html#sub:resolution-strategy)
문서를 보면 이 동작에 대해 잘 설명되어 있다.

### Shading

라이브러리 제작자는 의도적이든 아니든 이전의 버전에서 공개한 인터페이스를
깨뜨리게 된다. 이 경우 문제를 해결하기가 매우 힘들다. 이 때 문제를 회피하는 방법
중 하나로 [maven shade plugin](https://maven.apache.org/plugins/maven-shade-plugin/)이 있다. shade plugin을 사용하면 특정 라이브러리들의 package name을 바꿀 수 있다. 이렇게
라이브러리의 package를 다르게 하면 여러 버전의 라이브러리를 함께 사용할 수 있다.

Elasticsearch의 [To shade or not to shade](https://www.elastic.co/kr/blog/to-shade-or-not-to-shade) 블로그 글을 읽으면
어떤 경우에 shade를 쓰는지 더 잘 이해할 수 있다.

### 라이브러리를 작성하는 사람을 믿을 수 있는가

Java가 라이브러리를 다루는 방식은 Node.js나 Rust와는 방향성이 다르다고 느껴진다.
Rust나 Node.js는 Java에 비해 쉽게 라이브러리를 쓸 수 있다. 라이브러리 제작자도
이전 코드를 유지할 부담 없이 쉽게 새 버전을 만들고 인터페이스를 고칠 수 있다.

반면 Java에서는 라이브러리를 쓰면서 불안한 점이 많다. 라이브러리 제작자가 이전
버전 인터페이스를 깨면 사용하는 입장에서는 고치기가 매우 어렵다. 라이브러리를
만드는 사람은 새로운 버전, 새로운 인터페이스를 만들 때마다 이를 유지할 책임이
생긴다.

라이브러리 생태계를 보면 확실히 차이가 느껴진다. Node.js는 left pad같이 엄청
간단한 함수도 라이브러리를 가져다 쓰기도 한다. Java는 오픈소스 라이브러리 갯수도
적고, 하나의 라이브러리가 많은 일을 할 때가 많다. 아직 신뢰가 쌓이지 않은
라이브러리를 잘못 사용했다가 큰 고통을 받을 수 있기 때문인 거 같다.
