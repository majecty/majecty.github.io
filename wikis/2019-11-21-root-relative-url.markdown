---
title: 같은 웹 사이트 내의 다른 리소스의 위치는 root relative를 써서 가리키자
author: 주형
tags: web, url
---

절대 경로(Absolute)
--------

`https://blog.majecty.com/contact.html`

말 그대로 protocol부터 url의 path까지 모든 걸 적는 방법이다. 자신의 사이트가 아닌 다른 사이트를 가리킬 거 라면 이걸 써야겠지만, 자신의 사이트를 쓸 때는 쓰지 말아야할 방법이다. 왜냐하면 하나의 웹 서버가 여러 도메인에서 접근 가능하는 경우, 자신의 도메인이 아닌 다른 도메인을 가리키는 문제가 생기기 때문이다.

상대 경로(Relative)
--------

`../contact.html`

파일 경로에서 자주 사용하듯이 `..`을 사용해서 현재 보여지는 페이지로부터의 상대적인 경로를 표현하는 방식이다. 파일 시스템에서는 유용했지만 웹에서는 두가지 면에서 부적절하다. 첫 번째는 웹에서는 맨 뒤에 / 가 있을 수도 있고 없을 수도 있는 URL이 가능하기 때문에 URL에 따라 다른 리소스에 접근할 수 없는 문제가 생긴다는 문제이다. 예를 들어 blog.majecty.com/posts/index.html 과 blog.majecty.com/posts 는 같은 페이지를 가리킬 수 있지만 각 페이지에서 접근하는 상대 경로는 달라진다.

둘 째는 코드의 재활용을 할 때 재활용된 코드에서 다른 리소스에 접근하지 못하는 문제이다. 복잡한 웹사이트를 만들다 보면 웹페이지의 부분부분을 재활용하는 경우가 생긴다. 이 때 컴포넌트가 어떤 페이지를 상대경로로 가리킨다면, 그 컴포넌트를 다른 디렉토리의 페이지에서 재활용할 수 없다.

루트 상대 경로(Root relative)
--------

`/posts/a.html`

경로의 시작을 / 로 하는 것이 루트 상대경로이다. 이 방법을 쓰면 웹 페이지가 여러 도메인을 가질 때도 대응이 되며, 현재 페이지가 어떤 상태이든 원하는 페이지를 연결할 수 있다.

루트 상대 경로와 prepath
--------

종종 한 웹사이트를 sub url에서 서비스할 수도 있다. 예를 들어 블로그를 majecty.com/blog 에서 서비스한다고 했을 때 블로그 프로젝트의 모든 url은 /blog로 시작해야한다. 안타깝게도 쉬운 해결책은 없다. 자신이 사용한 모든 내부 url마다 찾아가서 /blog prefix를 붙여주어야한다. 이를 원하지 않는다면 처음부터 prefix변수를 template엔진에서 선언한 뒤 모든 url에서 그 변수를 사용해야한다.

gatsby는 React를 사용하는 static 웹사이트 생성기인로 prefix를 고려하고 있기 때문에 유저가 쉽게 prefix를 적용할 수 있다([링크](https://www.gatsbyjs.org/docs/path-prefix/)). 링크를 걸 때에는 `<Link />` 컴포넌트를 사용하기 때문에 prefix를 쉽게 바꿀 수 있으며, 다른 리소스들의 경우 webpack의 publicPath를 사용하여 쉽게 바꿀수 있다고 한다.

참고자료
--------

* [URL의 각 부분에 대한 설명](https://developer.mozilla.org/ko/docs/Learn/Common_questions/What_is_a_URL)
* [gatsby 프리픽스](https://www.gatsbyjs.org/docs/path-prefix/)
* [webpack의 public path](https://webpack.js.org/guides/public-path/)
