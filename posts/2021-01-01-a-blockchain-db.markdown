---
title: 블록체인이 사용하는 DB
author: 주형
tags: blockchain, bitcoin, ethereum, LSM Tree, 자료구조
---

내가 코드를 읽었던 블록체인 구현체들은 대체로 Log-structured merge-tree(LSM Tree) 기반의 key value 디비를 사용했다.
비트코인 코어는 LevelDB를 사용한다. 이더리움의 Go언어 구현체도 LevelDB를 사용한다.
Parity 이더리움(지금은 OpenEthereum)은 RocksDB를 사용한다. Cosmos도 LevelDB를 사용한다.

처음에는 RocksDB와 LevelDB가 SQL을 지원하지 않는 Key-Value형 DB라서 NoSQL들이 쓰는 자료구조인줄 알았다.
좀 더 찾아봤더니 SQL이든, NoSQL이든 상관없이 쓸 수 있는 자료구조였다.
내가 이전 프로젝트에서 썼던 elastic search도 LSM Tree를 쓰고 있었다.
MySQL에도 InnoDB대신 MyRocks는 스토리지 엔진을 사용하면 LSM Tree를 사용할 수 있다.
LSM Tree는 흔히 SQL 디비에서 사용하는 B-Tree에 대응되는 자료구조로 이해하면 된다.

블록체인이 RocksDB나 LevelDB같은 임베디드 디비를 쓰는 이유는 여러 가지가 있을 것이다.
블록체인 실행파일에 디비 코드가 같이 링크되므로 편하게 배포를 할 수 있다.
다른 프로세스들과 공유하지 않기 때무옉 블록체인 컨텍스트에 맞게 튜닝하기도 좋다.
하지만 내가 생각하는 가장 중요한 이유는 디비 구현체의 버전을 고정시킬 수 있기 때문이다.

2013년에 비트코인 코어 구현체는 0.8 버전으로 올리면서 Berkeley DB에서 LevelDB로
디비를 바꾸었다. 원치 않던 사이드 이펙트로 Berkeley DB에 있던 문제가 해결되었다.
이 때문에 네트워크가 0.8이전 버전과 0.8버전으로 나뉘는 심각한 문제가 있었다.[^1][^2]

[^1]:
  BIP 50 문서에 해상 이슈에 대한 포스트 모템 글이 정리되어 있다. [link](https://github.com/bitcoin/bips/blob/master/bip-0050.mediawiki)

[^2]: 비트코인과 이더리움 역사를 보면 재밌는 것들이 많다.

비트코인 코어는 LevelDB의 소스코드를 src/leveldb 서브디렉토리에 복사하여 본인들이 직접
코드 업데이트를 관리하고 있다. 이 [스택 오버플로우 답변](https://bitcoin.stackexchange.com/a/75147)을 보면
Window 지원과 체인 포크 방지를 위해 LevelDB를 포크했다고 설명하고 있다.
답변을 단 사람은 Pieter Wuille로 비트코인 코어 개발자다.

임베디드 디비로는 SQLite 역시 유명하다. SQLite 대신 LevelDB를 쓴 이유는 속도때문이라고 한다.
이 [스택 오버플로우 답변](https://bitcoin.stackexchange.com/a/48968)에서도 Pieter Wuille씨가
답변을 해주셨다. LSM Tree를 쓴다는 것 자체가 B-Tree에 비해 성능의 큰 이점은 아니라고 생각한다.
다만 SQLite는 SQL엔진을 올려서 값을 쓰기까지 복잡한 과정을 필요하지만, LevelDB는 key value 바이너리
데이터를 단순히 저장하기 때문에 그 차이가 나는 것 같다.

이 글을 쓰면서 Rocks DB 문서를 바탕으로 LSM Tree가 동작하는 방식을 [여기에](https://blog.majecty.com/wikis/2021-01-01-a-lsm-tree.html) 정리했다.

LSM Tree의 동작은 재밌는 점이 많다. 디스크 쓰기는 랜덤보다 시퀀셜이 항상 더 빠르다고 많이 들어왔다.
하지만 시퀀셜 쓰기만으로 의미있게 데이터를 저장하는 건 불가능하다고 생각했다.
LSM Tree는 그걸 해냈다. 랜덤 쓰기를 하지 않는다. WAL는 append-only고, SST 파일은 한 번 쓰면 수정하지 않는다.
종종 여러 SST 파일을 합쳐서 새로운 SST 파일을 쓴다. 그래서인지 B Tree 구현체들에 비해서 쓰기속도가 더 빠르다고 한다.[^3]

[^3]: [간단한 벤치마크](https://github.com/wiredtiger/wiredtiger/wiki/Btree-vs-LSM)

그리고 LSM 역시 데이터를 immutable하게 다룬다. 최근에 [Ethereum에서 사용하는 immutable 자료구조](https://blog.majecty.com/posts/2020-12-28-b-ethereum-immutable-data-structure.html)에서도
이야기 했지만 immutable한 방식을 만나면 반갑다. 그래 더 빠른 성능을 위해서 immutable을 선택할 수도 있다고.

