---
title: cocos creator에서 키보드 입력 받기
author: 주형
tags: cocos creator, game
---

2017년 3월 24일에 cocos creator 1.1.1 버전을 사용한 것을 바탕으로 작성한 글입니다.

cocos creator의 문서는 빠진 부분이 많습니다. 키보드 입력을 받는 부분도 문서에 없습니다. 아마 모바일 게임을 만들것이라고 가정해서 키보드 쪽 문서를 아직 작성하지 않은 것 같네요. 하지만 모바일에서도 키보드를 사용할 수도 있습니다. 웹게임이라면 키보드를 주 입력장치로 사용할 수도 있죠.

아래부터 코드를 보겠습니다.
(cocos2d engine쪽 코드를 읽고 돌아가는 방법을 찾았습니다. 버전업되면 다른 api로 바뀔지도 몰라요.)

```javascript
    // use this for initialization
    onLoad: function () {
        let listener = cc.EventListener.create({
            event: cc.EventListener.KEYBOARD,
            onKeyPressed: function (keyCode, event) {
                cc.log('pressed key: ' + keyCode);
            },
            onKeyReleased: function (keyCode, event) {
                cc.log("released key: " + keyCode);
            }
        });

        cc.eventManager.addListener(listener, this.node);
    },
```

보시면 코드는 간단합니다.

1. listener를 생성
2. 전역 singleton인 eventManager에 등록.

addListener의 두번째 인자는 event의 우선순위로써 숫자를 넘길 경우 빠른 숫자인 event부터 호출이 됩니다. 지금처럼 node를 넘길 경우 scene graph의 순서에 따라서 호출되게 됩니다.

등록된 event는 addListener의 두번째 인자로 넣은 node가 파괴될 때 자동으로 없어집니다.

addListener가 성공할 경우 listener를 다시 return해주는데 이 listener를 사용하여 이벤트만 없애고 싶을 때 eventManager.removeListener를 호출할 수 있습니다.


### 참고

* [addListener 소스 코드](https://github.com/cocos-creator/engine/blob/master/cocos2d/core/event-manager/CCEventManager.js#L749)
* [removeListener 소스 코드](https://github.com/cocos-creator/engine/blob/master/cocos2d/core/event-manager/CCEventManager.js#L810)

