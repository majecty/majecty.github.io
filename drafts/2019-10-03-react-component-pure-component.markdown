---
title: React Component와 Pure Component
author: 주형
tags: react, component, purecomponent
---

# Draft에 있는 이유. 리액트 홈페이지에 있는 글을 단순히 번역했기 때문. 내 의견이나, 다른 정보가 더 필요.

shouldComponentUpdate가 구현되어 있는 지 아닌지에 따라 다름. Render의 결과가 props와 state에만 의존할 때 React.PureComponent를 쓰면 성능상의 이점을 얻을 수 있음.

React.PureComponent는 얕은 비교를 하기 때문에, 복잡한 구조의 데이터에서 의도치 않은 결과가 나올 수 있음. 복잡한 값일 때는 Component를 쓰거나, 아니면 수동으로 forceUpdate()를 호출하거나, 아니면 immutable object를 써야함.

부모가 PureComponent면 자식도 항상 PureComponent여야함.

