---
title: Netty의 이해 - 왜 필요할까
author: 주형
tags: java, netty
summary: Netty를 사용하면 편하게 non blocking IO를 할 수 있다.
---

## nio는 불편해

Java의 nio를 사용해서 직접 non blocking IO 코드를 작성하는 건 불편하다. Java의
[nio 패키지](https://docs.oracle.com/en/java/javase/15/core/java-nio.html) 는
non blocking IO를 제공한다. non blocking IO는 네트워크로 데이터를 보내거나 받을
때 쓰레드가 블록킹 되지 않는 IO 처리 방식을 말한다. nio의 non blocking 기능을 사
용하면 한 쓰레드만 사용해서 여러 소켓에서 오는 데이터를 기다릴 수 있다.

## 여러 쓰레드 사용의 필요성

비지니스 코드에서 nio의 non blocking IO를 직접 사용하는 건 많이불편하다. 비지니
스 코드를 짜다 보면 다양한 IO가 필요하다. 하나의행동을 하기 위해 데이터베이스에
값을 쓰거나 외부 REST API 요청을보내야 한다. 일반적으로 해야할 일들이 순차적으로
있고 중간 중간에 IO 작업을 하게 된다.

non blocking IO는 아까 말했듯이 한 쓰레드가 여러 IO 이벤트 발생을기다리는 구조다
. 비지니스 로직은 실행하던 중간에 IO를 기다려야하고 IO가 끝나면 다시 실행해야 한
다. 비지니스 코드 흐름과 non blocking IO 처리 코드를 잘 관리하기는 꽤 힘들다.

## 그래서 Netty가 도와줌

Netty를 사용하면 Netty가 간결한 쓰레드 구조를 제공해준다. Netty는 `EventLoop` 타
입을 제공한다. 이 루프는 싱글 쓰레드로 동작하면서 필요한네트워크 이벤트를 기다리
기, IO 이벤트 이후 해야하는 일 처리를 해준다. 비지니스 코드에서는 네트워크 처리
이후 할 일을 콜백함수 형식으로 이벤트루프에 넘겨주면 된다. 이벤트 루프는 IO가 잘
처리된 이후 필요한 콜백을호출해준다.

콜백들이 항상 한 쓰레드에서 실행한다는 게 보장되기 때문에 비지니스 로직도쓰레드
걱정 없이 작성할 수 있다.

## 간단하게 Netty의 EventLoop를 쓰는 예제를 봐보자

다음처럼 이벤트 루프에 다음에 할 콜백들을 등록할 수 있다. Network 코드가 추가되
면 예시가 복잡해져서 `EventLoop`가 콜백을 호출하는 예시만작성했다
.[^eventloop-callback-example]

전체 코드는
[이 링크](https://github.com/majecty/any-study/blob/da100a5221982835a46258643c3465edd84ab3d2/java/netty/app/src/main/java/dev/juhyung/study/netty/eventloop/EventLoopExample.java)에
있다.

```java
public class EventLoopExample {

  public static void main(String[] args) throws InterruptedException {
    final var eventLoop = new DefaultEventLoop();
    eventLoop.schedule(
        () -> {
          System.out.println("run in 100 millisec later");
          eventLoop.shutdownGracefully();
        },
        100,
        TimeUnit.MILLISECONDS);

    eventLoop.submit(
        () -> {
          System.out.println("run immediately");
        });
    eventLoop.awaitTermination(1, TimeUnit.SECONDS);
  }
}

```

[^eventloop-callback-example]:
    사실 Netty를 사용하면 EventLoop를 직접사용할 일이 없다. 보통 ChannelHandler
    라는 interface를 사용한다.

## 그래도 자바인데 여러 쓰레드를 써야하지 않을까

당연히 멀티 쓰레드를 활용할 수 있다. 하나의 `EventLoop`는 하나의쓰레드만을 사용
하지만 하나의 Netty프로그램은 여러 `EventLoop`를사용한다. 예를 들어 400개의 소켓
을 관리한다면 100개의 소켓을 관리하는 4 개의 `EventLoop`를 사용한다.

## 결론

nio로 non blocking IO를 직접 하는 건 꽤 귀찮고, Netty를 사용하면 좀 더편하게 코
드를 작성할 수 있다.

## 참고

- 네티의 쓰레드 모델은 잘 설명된 공식 문서가 없다. 대신
  [Netty in Action](https://www.amazon.com/Netty-Action-Norman-Maurer-ebook-dp-B0977YYX1C/dp/B0977YYX1C/ref=mt_other?_encoding=UTF8&me=&qid=)책
  을 보면 잘 설명되어 있다.
