---
title: 유니티 UI에서 버튼 선택가능 영역 설정하기
author: 주형
tags: unity, 삽질
summary: 유니티 UI에서 버튼 선택가능한 영역을 설정하는 법 정리
---

# 깨진 유니티의 UI 추상화

유니티에서 UI는 Visual Component와 Interaction Component들로 나뉩니다. Visual component는 Image나 Text등 눈에 보이는 것을 만들어내는 컴포넌트들입니다. Interaction Component는 Button과 Scrollbar, Toggle 등 유저의 인터렉션을 처리하는 컴포넌트들입니다. Interaction Component는 홀로는 쓰일 수 없고 항상 무언가를 보여주는 Visual component와 함께만 사용가능합니다.

대부분의 경우 문제없이 동작하지만 요구사항이 디테일해지면 문제가 생길 수 있습니다. 버튼은 특정 영역을 누르면 반응이 나옵니다. 여기서 눌렸는 지 체크하는 영역은 그 버튼의 VisualComponent의 RectTransform에 의존합니다. 다시 말해서 VisualComponent들을 통해서만 버튼의 클릭 영역을 설정할 수 있습니다.

반짝반짝 하는 이펙트가 함께 달린 버튼 이미지를 사용한다면 반짝반짝 하는 이펙트 부분까지 전부 클릭 가능한 영역이 됩니다. 직관적인 방법을 통해선 보이는 영역보다 작게 클릭 가능한 영역을 설정할 수 없습니다. 그리고 모든 이미지는 사각형이기 때문에 사각형 이외의 클릭 영역을 만들 수 없습니다. 원 모양의 클릭 영역을 만들어낼 수 없습니다.

개념을 단순하게 하기 위해서 보이는 영역과 인터렉션 가능한 영역을 합쳐버린 게 문제인거죠. 따라서 보이는 것과 인터렉션 가능한 영역이 다를 때를 처리하지 못합니다.

# 우회해서 해결하기

조금 귀찮은 작업을 하면 보이는 것보다 작은 인터렉션 영역을 만들 수 있습니다. 저는 다음과 같은 구조를 만들어서 우회했습니다. 버튼이 있는 게임오브젝트, 화면에 보이는 Image를 가지는 게임오브젝트, 클릭할 영역을 가지는 게임오브젝트로 3개를 분리했습니다.

```
buttonGameObject(Button component)
  - imageGameObject(Image component without Raycast Target)
  - clickableGameObject(Image component with transparent image)
```

buttonGameObject는 버튼을 나타내는 상위 게임 오브젝트입니다. 이 게임 오브젝트는 버튼 컴포넌트를 가지고 있습니다.

그 자식으로 image게임 오브젝트를 만듭니다. 이 게임 오브젝트는 Image 컴포넌트를 가지고 화면에 보이는 이미지를 세팅합니다. 그리고 버튼의 선택가능한 영역에 속하지 않도록 raycastTarget	을 끕니다.

다음으로 clickableGameObject를 buttonGameObject의 자식으로 만듭니다. 이 gameObject도 Image컴포넌트를 가집니다. 이 게임 오브젝트는 클릭가능한 영역을 위해서 만든 것이므로 source
image를 비워두고 Color에서 alpha를 조절해서 투명하게 만듭니다. 클릭이 가능해야하므로 raycastTarget옵션을 키고, rectTransform을 조절해서 원하는 만큼만 클릭이 가능하게 만듭니다.

투명한 이미지를 사용하여 클릭가능한 영역을 설정한다는 방식이 좋아보이진 않지만 이 방법을 사용하면 보이는 것보다 더 작은 영역에서 클릭이 가능하게 만들 수 있습니다.
