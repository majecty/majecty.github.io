---
title: Solana anchor에 대한 인상
author: 주형
tags: solana, anchor
summary: anchor 를 사용하면서 느낀 점
---

solana 에서 program[^1] 을 만들 때 [anchor](https://www.anchor-lang.com/docs) 를 사용하면 직접 작성하기 까다로운 코드를 쉽게 작성할 수 있습니다. 최근 solana program을 직접 작성하면서 anchor 를 사용해본 경험을 정리했습니다.

### solana의 account
solana 블록체인에 저장하는 모든 값은 account 라는 단위로 저장합니다. 모든 account 는 블록체인 지갑처럼 고유한 publickey 로 구분할 수 있습니다. 솔라나 프로그램의 코드도, 솔라나 프로그램이 저장하는 state도 각각의 account 에 저장해요. 블록체인에 값을 저장하는 건 비싸기 때문에 solana account 를 만들거나 크기를 키울 때 꽤 많은 sol을 예치금으로 담아두어야합니다.

account 를 다루는 건 까다로운 지점이 많습니다. 저장하고 있는 값의 byte 수에 따라서 solana 를 예치해야하기 때문에 크기가 변하는 값을 저장하기가 어려워요. map 같은 걸 만드는 건 불가능에 가깝고, size가 바뀌는 array도 어렵습니다. solana가 내부적으로 byte array로만 값을 저장해주기 때문에 프로그래머가 알아서 값들을 직렬화/역질렬화를 해야합니다. account 관련 보안 문제가 될 지점도 많아서 조심히 써야합니다. 모든 account 는 solana program 외부에서 전달되어 들어옵니다. 악의적으로 엉뚱한 account를 인자로 주었을 때 검증을 안하면 자산을 탈취당하기 쉽습니다.

이렇게 다루기 귀찮은 account 방식을 쓰는 이유는 transaction을 병렬적으로 실행하기 위해서에요. 여러 트랜잭션이 서로 다른 account 를 읽거나 쓰는 경우는 안전하게 병렬적으로 실행할 수 있어요. solana의 transaction은 자신이 읽거나 쓰는 모든 account를 미리 명시하고 있기 때문에 transaction을 실행하기 전에 병렬적으로 호출할 transaction들을 미리 파악할 수 있습니다. 다른 스마트컨트랙트 언어인 move 언어나 telegram의 ton 블록체인 역시 병렬 실행을 위해 비슷한 구조를 사용해요.

### 불편한 account 를 anchor로 쉽게 다루기

anchor를 사용하면 직렬화/역직렬화 코드와 보안을 위한 account 검증 코드를 쉽게 작성할 수 있어요. anchor 가 제공해주는 매크로를 사용하면 solana struct 로 정의한 account 의 직렬화/역직렬화 코드가 자동으로 생성됩니다. anchor 의 규칙에 맞게 함수를 정의하면 사용하기 쉽게 solana struct 를 인자로 받아올 수 있어요.

아래 코드는 solana spl token program의 일부분이에요[^2]. transfer instruction을 처리하는 코드의 앞부분과 끝부분입니다. account 목록이 `accounts` 인자로 들어오면 이걸 하나씩 꺼낸 뒤 `Account::unpack` 이라는 함수로 역직렬화를 하는 걸 볼 수 있어요. 모든 로직이 끝나면 다시 Account::pack 함수를 호출해서 byte array로 직렬화하고 있습니다.

```rust
/// transfer 코드 앞부분
pub fn process_transfer(
	program_id: &Pubkey,
	accounts: &[AccountInfo],
	amount: u64,
	expected_decimals: Option<u8>,
) -> ProgramResult {
	let account_info_iter = &mut accounts.iter();

	// account_iter로부터 account를 하나씩 꺼내옵니다.
	let source_account_info = next_account_info(account_info_iter)?;
	....
	let destination_account_info = next_account_info(account_info_iter)?;
	...

	// 꺼낸 account_info를 unpack해서 읽고 쓰기 쉬운 struct 타입으로 변환합니다.
	let mut source_account = Account::unpack(&source_account_info.data.borrow())?;
	let mut destination_account = Account::unpack(&destination_account_info.data.borrow())?;
```


```rust        
	// transfer 코드 뒷부분
	
	Account::pack(source_account, &mut source_account_info.data.borrow_mut())?;
	Account::pack(
		destination_account,
		&mut destination_account_info.data.borrow_mut(),
	)?;

	Ok(())
}

```

아래 코드는 anchor 를 사용한 코드입니다. account 를 pack / unpack하는 과정을 anchor 가 자동으로 해주기 때문에 account 를 사용하는 코드가  무척 간결해집니다. 그 이외에도 mutable 설정에 대한 검증이나, account 의 ownership[^3] 에 대한 검증들도 자동으로 처리해줍니다.

```rust
 
#[derive(Accounts)]
pub struct TransferAccounts<'info> {
    #[account(mut)]
    pub source: Account<'info, TokenAccount>,

    #[account(mut)]
    pub destination: Account<'info, TokenAccount>,
    ...
}

pub fn process_transfer(
	ctx: Context<TransferAccounts>,
	amount: u64,
	expected_decimals: Option<u8>,
) -> ProgramResult {
	let mut source_account = &ctx.accounts.source;
	let mut destination_account = &ctx.accounts.destination;

    // pack을 명시적으로 하지 않아도 anchor 가 알아서 처리해줍니다.
```

### Anchor 의 아쉬운 점들

anchor 는 편리했지만 조금만 사용해도 불편함을 크게 느낄 수 있어요. 가장 먼저 어려웠던 점은 암묵적인 규칙들에 대한 에러메시지가 매우 불친절하다는 점입니다. anchor 의 문서에서 가장 중요한 부분은 [Account Types](https://www.anchor-lang.com/docs/references/account-types) 와 [Account Constraints](https://www.anchor-lang.com/docs/references/account-constraints) 파트입니다. 다양한 account type 과 account 의 constraint 를 골라서 사용해야합니다. 예를 들어서 아래처럼 token::mint 에 target_account 를 넣어야할 때 어떤 값을 어떻게 넣어야하는지에 대해서 알려주지 않아요. target_account에 상수를 넣을 수 있는지, 같은 accounts 안의 다른 field를 넣을 수 있는지, 다른 account의 안의 data에 적혀있는 account address 를 써도 되는지 알 수 없어요. 어떤게 될지 몰라서 임의의 값을 넣어보더라도 에러메시지가 매우 불친절해서 어떻게 고쳐야하는지 알기 어렵습니다. 몇 번 시도하다 보면 결국 example 에 작성된 코드에서 조금이라도 벗어나는 코드를 쓰기 어려워져요.

```rust
#[account(
    token::mint = <target_account>,
    token::authority = <target_account>
)]
 
```

또 다른 예시로 optional account 가 있습니다. 아래 코드처럼 optional 하게 account 를 받아올 수 있어요. 문제는 이 accounts를 typescript 에서 생성해야할 때 문제가 발생했어요. solana 는 [web3.js library](https://www.npmjs.com/package/@solana/web3.js) 에서 [solana kit library](https://github.com/anza-xyz/kit) 로 이전하고 있어요. anchor 가 만들어주는 typescript library 는 web3.js 에 의존하고 있어서 제가 작업하는 프로젝트에서 쓸 수 없었습니다. 따라서 이 optional 한 필드에 값을 빼고 싶을 때 어떠한 처리를 해야하는지 제가 직접 확인하고 작업을 해야했어요. 하지만 anchor 쪽에서는 optional한 field를 어떻게 처리하는지 문서가 없어요. 그리고 solana에서는 account 목록을 transaction에 담을 때 optional한 인자를 처리하는 방법을 제공해주지 않아요. anchor가 임의의 규칙을 만들어서 optional 처리를 하고 있던 거에요. 결국 optional 을 어떻게 처리하는지는 anchor 내부 코드를 읽고 나서야 이해할 수 있었습니다.

```rust
#[derive(Accounts)]
pub struct InstructionAccounts<'info> {
    pub account: Option<Account<'info, AccountType>>,
}
```

아래가 anchor 내부에서 optional한 타입을 처리하는 코드입니다. account 의 key가 pogram의 id를 넣어주면 None 으로 처리해주는 거였어요. 이 [링크](https://github.com/solana-foundation/anchor/blob/1ebbe58158d089a2a40b5e35ebead5a10db9090d/lang/src/accounts/option.rs#L46)에서 코드를 직접 보실 수 있습니다.

```rust
if accounts[0].key == program_id {
	*accounts = &accounts[1..];
	Ok(None)
} else {
	// If the program_id doesn't equal the account key, we default to
	// the try_accounts implementation for the inner type and then wrap that with
	// Some. This should handle all possible valid cases.
	T::try_accounts(program_id, accounts, ix_data, bumps, reallocs).map(Some)
}
```

### Anchor 를 어떻게 다루어야할까

내가 작성하는 solana program이 anchor 에서 제공해주는 예시를 따라하는 것 만으로도 요구사항이 만족되면 딱히 문제가 없습니다. 하지만 그 이외의 작업을 해야한다면 결국 anchor 의 소스코드를 읽어야하고, `anchor expand` 를 사용해서 anchor 가 생성하는 소스코드를 확인해야합니다. (anchor expand 명령어로 anchor가 생성하는 코드를 출력해볼 수 있어요.) 안전한 program을 작성하는 데 큰 도움을 주는 라이브러리이긴 해요. 어떠한 라이브러리들은 디테일을 잘 감싸주어서 사용자들이 디테일을 잘 모르는 상태로 써도 됩니다. anchor는 반대로 사용하는 사람들이 anchor가 해주는 데테일이 무엇인지 전부 파악한 상태로 사용해야하는 라이브러리입니다.

[^1]: solana blockchain의 smart contract 를 program이라고 부릅니다. 코드와 state 가 solana blockchain에 저장됩니다. solana program의 결과는 다시 state 에 적혀서 solana 를 사용하는 모든 사람이 확인할 수 있습니다.

[^2]: https://github.com/solana-program/token/blob/369b0818ee958e52e9110bcf0842ecceabd7c28c/program/src/processor.rs#L227 에서 원문 코드를 확인할 수 있습니다.

[^3]: 모든 accounet는 owner account 를 지정합니다. 이 owner account 에 담긴 program만이 account 에 적힌 data 값을 수정할 수 있어요.
