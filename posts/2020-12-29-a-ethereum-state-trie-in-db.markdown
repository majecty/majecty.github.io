---
title: Ethereum의 state trie는 hash를 포인터로 쓴다
author: 주형
tags: blockchain, ethereum
---

이더리움의 스테이트 트라이를 처음 공부할 때, 잘 이해가 안가던 부분이 있었습니다.
바로 트라이 노드간의 레퍼런스를 어떻게 표현하는지,
그리고 그 연결 부분이 디스크에 어떻게 저장되는지였습니다.

트라이는 트리의 한 종류입니다. 이 글에서는 이더리움 스테이트 트라이의 트리적인 특징에 대해서만
이야기 하겠습니다.

제가 그 때까지 알던 트리를 메모리에서 표현하는 방법은  두가지가 있었습니다. 각 노드를 힙에 할당한 뒤 자식 노드에 대한
포인터를 부모 노드에 저장합니다. 다른 방법은 perfect binary tree에서 각 노드를 어레이에 순서대로
저장하는 방법입니다. 이 방법을 쓰면 index의 연산으로 쉽게 자식 노드를 찾을 수 있습니다.

트리를 디스크에 저장할 때는 메모리의 표현 방식에 상관 없는 방법을 썼습니다.
정렬된 트리였다면 정렬된 원소들을 리스트 형태로 디스크에 저장했습니다.
아니면 트리 구조를 나타낼 수 있는 포맷인 JSON 이나 XML 같은 방법을 쓸 수 있구요.
SQL DB에 저장하는 데이터는 SQL에 저장할 때 생성한 ID를 레퍼런스로 썼습니다.
이 경우 메모리에 올릴 때도 해당 ID로 노드를 찾을 수 있는 트리나 해시테이블을 썼습니다.
아니면 메모리에 올릴 때 정해지는 메모리 포인터를 쓸 수도 있습니다.

-----

이더리움 스테이트 트라이는 놀랍게도 자식 노드의 Hash를 레퍼런스로 사용합니다.
메모리에 있을 때도 트라이의 모든 노드를 키밸류 자료구로(해시나 트리)에 저장합니다.
부모 노드는 자식노드의 해시값을 가지고 있어서 자식 노드의 해시를 키로 자식을 찾아옵니다.

트리를 구현하기 위해 또 다른 트리(혹은 해시테이블)을 쓰다니 저에게는 혁명적인 발상이었습니다.
한 번 더 재밌는 건 메모리에서의 표현 방법과 디스크에서의 표현 방법이 동일하다는 점입니다.
디스크에 저장할 때도 Level DB나 Rocks DB같은 key value 스토리지에 각 노드를 저장합니다.
메모리든 디스크든 구분할 필요가 없습니다.

지금 이더리움의 state trie는 일반 컴퓨터의 메모리에 담기엔 너무 큽니다.
자주 접근하는 노드는 메모리의 key value 스토리지에 캐시처럼 저장하고, 자주 안쓰는 데이터는
디스크에서 그때 그때 읽어오는 방법을 씁니다.

저는 항상 트리에서 노드들끼리 연결하는 방식은 디스크에 있을 때와 메모리에 있을 때 서로
다르게 표현될 것이라는 고정관념이 있었습니다. 이 틀에 맞지 않는 구조라 처음에 이해하기가 어려웠던 것 같네요.

말 안하고 넘어가면 아쉬우니 하나 추가하자면, 이렇게 자식 노드의 해시를 노드의 포인터로 취급하여 같이 저장하기 때문에 최상위 부모 노드의 해시는 트리 전체의 해시를 한 것과 같습니다. 아마 이 특징을 얻기 위해서
해시 값을 포인터로 쓴 것이라고 생각합니다. 블록체인에서 전체 상태의 해시값을 구하는 건 중요하니까요.
