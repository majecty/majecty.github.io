---
title: 조심히 써야 하는 React.PureComponent
author: 주형
tags: react, component, purecomponent
---

# 요약
React.PureComponent를 사용하면 컴포넌트가 참조투명하다는 표지를 함으로써 불필요하게 렌더가 이루어지는 것을 막을 수 있다. 입력값이 바뀌었는지를 얕은 비교를 사용하기 때문에 실수하기가 쉽다. 컴포넌트가 원치 않게 업데이트하지 않는 것은 찾기 힘든 버그이기 때문에 꼭 필요한 상황이 아니면 쓰지 않는 것이 좋다.

# PureComponent
React.PureComponent는 한 컴포넌트의 렌더 결과가 그 컴포넌트의 props와 state에만 의존하는 경우 사용할 수 있는 최적화용 class다. React.Component에 shouldComponentUpdate 함수를 구현하여, props와 state값이 바뀌지 않았다면 전에 render했던 결과물을 다시 사용한다. PureComponent는 ShallowCopy를 사용하여 props와 state값이 바뀌었는지 확인한다.

# 얕은 비교
값이 같은지 비교하는 건 어려운 일이다. 우리는 두 값을 비교할 때, object나 array같은 복합적인 값은 안쪽 값들까지 전부 비교가 되길 바란다. 똑같은 너구리 라면 두 봉지가 있다면 둘 다 오동통한 면발을 가지고 있어야지, 한쪽에 신라면 면발이 들어있으면 안 된다. 이렇게 복합적인 값의 내용까지 전부 비교하는 방법을 깊은 비교라고 한다. 문제는 비교하던 중 레퍼런스로 자기 자신을 가리키는 경우이다. 속의 값을 계속 타고 내려가기 때문에 비교가 끝없이 반복된다.

따라서 단 하나의 완벽한 `perfectCompare`함수는 자바스크립트엔 없다. 필요할 때마다 적절한 비교함수를 만들어야한다. PureComponent는 ShallowCopy를 사용해서 state와 props의 첫 단계 필드에 대해서 `is` 함수를 호출한다. state나 props로 어떤 값이 올지 모르기 때문에 적절한 선(1단계 깊이)까지만 비교한다. 이 비교는 state와 props의 필드가 number나 string같은 단순한 타입일 때 잘 동작한다. state와 props의 필드가 object나 array타입이면, 안쪽 필드 값이 바뀌는 것을 확인할 수 없다.

# 생길 수 있는 문제들
지속 가능한 개발을 위해서 신경 쓸 거리를 계속 줄여나가야 한다. React문서를 보면, PureComponent를 사용할 때에는 state나 props에 간단한 타입들만 사용하거나, immutable type을 권장하고 있다. 위 방법들은 프로그래머가 코드를 계속 신경 쓰게 만든다. 코드를 읽다가 PureComponent를 만나면, state나 props가 간단한 타입을 쓰고 있는지 확인해야 한다. immutable js를 써도 안심할 수 없다. 새로운 state나 props 값을 생성하면 기존 값과 비교했을 때 다르다고 나와야하기 때문이다.(immutable js에서 비교를 한다면 .equals 함수를 써야 하나, reactjs에서는 Object.is로 비교를 한다.) PureComponent를 사용하면 성능의 이득을 얻지만, 버그가 더 잘 생겨나고, 코드를 읽는 비용이 많이 들게 된다.

# 문제를 다시 해결한다면
문제의 원점은 안전하게 비교를 할 수 있는지 여부이다. JavaScript에서는 타입이 없고 객체가 실행 중 언제든지 필드 값의 타입이 바뀔 수 있기 때문에 안전한 비교가 사실상 불가능하다. 하지만 타입시스템을 강하게 적용하여 해당 타입이 Recursive한지 컴파일타임에 확인할 수 있다면 문제를 풀 수 있다. state나 props의 타입이 Recursive하지 않을 때만 equals 함수를 자동으로 생성하고, PureComponent를 상속받을 수 있게 하면 안전하게 PureComponent를 사용할 수 있다.

# 결론
PureComponent는 순수 펑셔널 컴포넌트임을 명시해서 성능을 최적화하지만 완전하지 않은 비교함수 때문에 실수할 여지가 있어서 조심히 사용해야 한다.