---
title: solana program이 할 수 있는 일과 할 수 없는 일
tags: solana, blockchain
summary: svm의 opcode와 syscall을 통해 solana program의 동작 원리와 한계를 이해하기
---

solana program[^solana-program]은 일반적인 코드를 작성하는 환경과 꽤 다릅니다. 이 다름을 알지 못하고 solana program 작성을 시작하면 나중에서야 다 뒤엎고 다시 작성하게 됩니다. solana program이 무엇을 할 수 있고, 무엇을 할 수 없는지 이해할 수 있도록 이 글에 정리해보았습니다.

[^solana-program]: solana 에서 동작하는 스마트 컨트랙트를 solana program이라고 합니다. 블록체인에 모든 상태가 기록되어있고, 트랜잭션을 통해서 상태를 변경할 수 있습니다.

모든 solana program은 SBPF virtual machine 라는 가상 머신의 opcode 로 컴파일 됩니다. 전체 opcode 는 [anza-xyz github repo의 doc](https://github.com/anza-xyz/sbpf/blob/ea33a5c236a5e476a5c64ebba873e40733de761a/doc/bytecode.md)에서 확인하실 수 있어요. 아래처럼 5가지 일을 하는 opcode들로 볼 수 있습니다.

* memory에 있는 값을 register 로 복사
* register에 있는 값을 memory에 복사
* register 들끼리 연산하기
* jump / call 등의 control flow 변경
* syscall 호출

그렇다면 syscall 은 어떤 것들이 있는지 찾아보겠습니다. [agave/sysalls/src/lib.rs 파일](https://github.com/anza-xyz/agave/blob/7fb1e0dda9c954618f1aef68c1aff6bc79e9d687/syscalls/src/lib.rs#L335)에서 찾아볼 수 있습니다. 다음 기능들을 사용하는 것으로 분류할 수 있습니다.

- log 남기기
- abort/panic
- pda[^pda] 계산하기
- crypto 함수(hash나 elliptic curve 함수들)
- 블록체인 정보 조회(시간, fee, 조회 등)
- cross program invocation(다른 solana program 호출)

[^pda]: solana에서 program이 관리하는 account를 pda라고 합니다. solana program 코드는 상태를 가지지 못합니다. 대신 pda를 만들어서 pda 안에 상태를 저장할 수 있습니다. 혹은 권한 관리가 필요할 때 여러 pda를 만들어서 pda마다 서로 다른 권한을 부여하기도 합니다.

저는 이 둘을 찾아보고 나서 궁금한 점이 생겼습니다. blockchain 의 transaction은 blockchain 에 있는 정보를 수정할 때 가치가 있습니다. 하지만 위의 instruction과 syscall 만 봐서는 blockchain이 저장하는 데이터를 읽고 쓸 방법이 없습니다. 어떻게 되는 걸까요. solana 에서는 모든 정보를 account에 담아서 관리합니다. transaction을 실행하기 전에 transaction이 읽고 쓰는 모든 account 를 미리 메모리에 올려둡니다. 이 메모리 번지수를 solana program에 넘기면 solana program이 memory에 있는 값을 읽고 쓰게 됩니다. transaction이 끝나면 변경된 메모리 값을 blockchain state에 저장합니다. 이를 이해하게 되면 solana transaction을 만들 때 왜그렇게 많은 account 목록을 미리 준비해야하는지 알게됩니다.

조금 재밌는 점도 찾아볼 수 있습니다. solana에서는 모든 account가 sol을 소유합니다. account가 얼마의 sol을 가지고 있는지는 account에 적혀있는 정보입니다. solana에 있는 program들이 account에 담기 sol balance를 수정할 수 있습니다.[^solana-balance-modification] 처음 보고 놀랐습니다. solana balance를 수정할 수 있게 하면서 어떻게 돈이 복사되지 않게 막을 수 있을지 궁금했습니다. 정답은 transaction 실행 앞뒤로 account 들의 solana balance 합이 맞는지 체크하기 때문에 돈이 복사되지 않는다는 것이었습니다. transcation을 실행하기 전에 transaction이 읽고 쓰는 모든 account의 solana balance 합이 transaction이 끝난 뒤 solana balance 합과 같은지 검증합니다. 검증하는 코드는 [agave/svm/src/transaction_processor.rs 파일](https://github.com/anza-xyz/agave/blob/7fb1e0dda9c954618f1aef68c1aff6bc79e9d687/svm/src/transaction_processor.rs#L951)에서 찾아볼 수 있습니다.

[^solana-balance-modification]: 모든 account는 owner account 를 지정합니다. 이 owner account 에 담긴 프로그램만이 account가 가진 solana 양을 줄일 수 있어요. 그리고 모든 solana 프로그램은 임의의 account 의 balance 값을 더 높게 수정할 수 있습니다. 자세한 내용은 [공식 링크](https://solana.com/docs/core/accounts)에서 확인할 수 있습니다.

이렇게 vm과 syscall 이 할 수 있는 일들을 이해하게 되면 solana program을 작성할 때 자신있게 코드를 작성할 수 있습니다. 내가 적성하는 코드가 어떻게 해석될지 이해하고 코드를 작성하면 더 안전하고, 더 효율적인 코드를 작성할 수 있습니다.
