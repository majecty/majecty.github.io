---
title: 모바일로 빌드했더니 앱이 꺼져요
author: 주형
---

Awake와 Start함수의 순서는 보장되지 않습니다.
다른 플랫폼으로 빌드했을 때 그 이전과 순서가 다를 수 있습니다.
찾기 힘든 버그가 발생할 수 있어요.

방법 1. 초기화를 담당하는 걸 만든다.
방법 2. 스크립트간의 순서를 지정한다.
