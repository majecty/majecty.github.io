---
title: 2022년 풀 스크린 웹사이트 해상도 대응하기 - 1
author: 주형
tags: web
summary: 모바일 웹페이지에서 게임처럼 해상도를 대응하는 방법을 알아보자.
---

인터렉션이 중요한 웹사이트는 게임 같은 해상도 대응이 필요하다.
[대왕트래블2020](https://bigkingtravel.com)은 2020년에 내가 외주 작업한 웹사이트
인데, 일반 웹페이지보다는 게임에 더 가깝다. 항상 전체 화면을 가득채우며, 화면 스
크롤은 쓰지 않는다. 작업하면서 게임 만들던 느낌이 많이 났다.

게임과 웹사이트를 둘 다 만들었었는데 둘이 화면을 보는 관점이 달라서 재밌었다.

### 웹사이트의 방식

웹사이트의 경우 컨텐츠가 잘 보이는 것에만 관심을 가진다. 컨텐츠를 적당히 블록 단
위로 나누고, 해당 블록을 가로로 배치할지 세로로 배치할지를 화면 크기에 따라 바꾼
다. 때로는 작은 화면에서는 몇가지 요소를 가리기도 한다.

아래는 모바일에서 보이는 내 블로그 화면이다. 왼쪽 오른쪽 여백 이외에는 글로 가득
채워져있다. 위에서 아래로 글이 계속 이어지며 한 화면을 넘치는 내용은 스크롤을 통
해 이어서 볼 수 있다.

<figure>
<img src="/images/2022-01-22-a/majecty-blog-mobile.png" height="200px">
<figcaption> 예시 이미지 - 내 블로그
</figcaption>
</figure>

<!-- ![예시 이미지 - 내 블로그](/images/2022-01-22-a/majecty-blog-mobile.png) -->

twitter bootstrap을 쓰면 grid layout을 쓸 때 화면 크기에 따라 특정 요소가 배치되
는 방식을 쉽게 바꿀 수 있다. 가로 길이가 큰 화면에서는 한 가로 줄에 2 요소를 배
치하다가가로 길이가 작아지면, 한 가로 줄에 하나씩만 배체할 수 있다.

[twitter bootstrap의 responsive design에 관한 문서 링크](https://getbootstrap.com/2.0.2/scaffolding.html#responsive)를
보면 twitter bootstrap이 여러 화면을 처리하기 위한 방법이 적혀있다.

### 게임의 방식

게임은 어떤 화면에서든 화면을 가득 채운다. 화면을 가득 채우는 배경이 있고 UI 요
소들이 화면의 가운데 혹은 네 모서리에 위치한다. 게임은 주로 가상의 세상을 다루고
, 가상의 세상은 크기가 무한하기에 알맞은 방식이다.

솔리테어 덱드아웃은 모바일 게임이다. 화면 맨 위와 아래에 UI가 있으며 그 안을 배
경이미지가 채운다.

<!-- ![예시 게임 이미지 - 솔리테어 덱드아웃](/images/2022-01-22-a/solitare-decked-out.png) -->

<figure>
<img src="/images/2022-01-22-a/solitare-decked-out.png" height="200px">
<figcaption> 예시 게임 이미지 - 솔리테어 덱드아웃
</figcaption>
</figure>

몬스터헌터 월드는 3차원으로 구성된 세계를 다룬다. 화면의 크기가 얼마이든 화면 전
체를 이 3차원 세계가 채운다.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/wL5NiOc64ag" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## 게임같은 웹사이트와 이에 대한 대응

웹페이지지만 게임처럼 새로운 세상을 담아야하는 웹사이트들도 있다. 이러한 웹사이
트는 일반적인 컨텐츠기반 웹사이트가 하듯이 화면을 채울 수 없다. 게임에서 사용하
는 방식을 따와서 웹에서 이를 만들어보자.

### 레터박스

가장 단순하고 쉬운 방식은 레터박스를 쓰는 것이다. 하나의 해상도를 선택하고, 그해
상도와 다른 화면에서른 까만 네모로 빈 칸을 채우는 것이다. 여러 해상도를 대응할순
없지만, 적어도 원하는 모든 이미지와 UI가 화면에 나오게 할 수 있다.

레터박스마저도 안한다면, 오른쪽 위 모서리에 있는 메뉴 버튼이 어떤 핸드폰에서는안
나오게 될 수도 있다.

### 레터박스 코드 예시

아래 코드를 [이 코드펜 링크](https://codepen.io/majecty/pen/mdBNExO)에서 직접 실
행해볼 수 있다.

<p class="codepen" data-height="300" data-default-tab="css,result" data-slug-hash="mdBNExO" data-editable="true" data-user="majecty" style="height: 300px; box-sizing: border-box; display: flex; align-items: center; justify-content: center; border: 2px solid; margin: 1em 0; padding: 1em;">
  <span>See the Pen <a href="https://codepen.io/majecty/pen/mdBNExO">
  letterbox-16-9</a> by majecty (<a href="https://codepen.io/majecty">@majecty</a>)
  on <a href="https://codepen.io">CodePen</a>.</span>
</p>
<script async src="https://cpwebassets.codepen.io/assets/embed/ei.js"></script>

HTML 코드

```html
<div>
  <div id="a169"></div>
</div>
```

CSS 코드

아래는 레터박스를 구현한 코드다. calc와 vw, vh를 사용해서 가로 세로 비율을 유지
시켰다. margin-left, margin-top을 통해서 검은 배경을 양쪽에 균등하게 배치한다.
`@media` 태그를 사용해서 화면의 비율에 따라서 다른 css가 적용되게 만들었다.

```css
body {
  margin: 0;
  padding: 0;
  /*
  화면 바깥 영역을 검은색으로 칠한다.
  */
  background: black;
}

/*
  가로 16, 세로 9 비율보다 가로가 큰 경우
  height를 꽉 채우고 width를 height * 16/9로 맞춘다
  화면의 가운데에 두기 위해서 margin-left에 적절한 겂을 넣는다.
*/
#a169 {
  background: blue;
  width: calc(16 / 9 * 100vh);
  height: 100vh;
  margin-left: calc(50vw - 16 / 9 * 50vh);
}

/*
  가로 16, 세로 9 비율보다 세로가 큰 경우
  width를 꽉 채우고 height를 width * 9/16으로 맞춘다.
  화면의 가운데에 두기 위해서 margin-top을 적절한 값으로 넣는다.
*/
@media (max-aspect-ratio: 16/9) {
  #a169 {
    background: red;
    width: 100vw;
    height: calc(100vw * 9 / 16);
    margin-top: calc(50vh - 9 / 16 * 50vw);
    margin-left: 0;
  }
}
```

### 다음은

우리는 단순 레터박스보다 더 잘할 수 있다. 꼭 보여야하는 영역과 안 보여도 되는 영
역을 나눔으로써 더 많은 해상도를 대응할 수 있다. 그에 대한 내용은 다음 글에서 다
루기로 한다.
