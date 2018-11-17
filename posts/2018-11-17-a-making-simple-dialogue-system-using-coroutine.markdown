---
title: 코루틴으로 간단하게 대화창 만들기
author: 주형
tags: unity3d, unity, game
---

코루틴을 사용하면 기본적인 대화창을 직관적으로 구현할 수 있습니다.

대화창은 매우 많은 게임에서 사용됩니다. 비주얼 노벨은 물론, RPG, 액션, 퍼즐 게임 등 말을 할 수 있는 캐릭터가 나오는 게임이라면 플레이어에게 정보를 제공하기 위하여 대화창을 사용합니다. 유저들 역시 게임에 대한 정보를 얻는 과정으로써 대화창에 익숙합니다.

하지만 유니티는 대화창 시스템을 제공하지 않습니다. 프로그래머가 직접 기본 UI로 만들거나 유료로 판매하는 대화창 시스템을 사용해야 합니다. 대화창에 대한 요구사항이 복잡하다면 기능이 많고 복잡한 유료 에셋을 쓰겠지만, 간단한 요구사항이라면 직접 만드는 게 낫습니다.

대화창의 스펙을 정해봅시다.

유저의 입력이 없는 경우를 먼저 생각해봅시다.

1. 한 줄의 대화를 한 글자씩 유저에게 보여준다.
1. 모든 글자를 보여준 뒤 잠시 기다린다.
1. 다시 다음 줄의 대화를 한 글자씩 보여주기 시작한다.

이 정도 스펙은 간단합니다. 다음과 같은 방식으로 만들 수 있겠네요.

```csharp
IEnumerator Run()
{
    for (int i = 0; i < texts.Count; i += 1)
    {
        yield return PlayLine(texts[i]);
    }
}

IEnumerator PlayLine(string text)
{
    for (int i = 0; i < text.Length() + 1; i += 1)
    {
        yield return new WaitForSeconds(0.05f);
        uiText.text = text.Substring(0, i);
    }

    yield return new WaitForSeconds(3f);
}
```

하지만 대부분의 유저는 게임의 대화창을 읽고 싶어 하지 않습니다. 최대한 빨리 대화창을 넘기고 게임을 하고 싶어 하죠. 그렇다고 대화창 전체를 스킵시켜버리면 중요한 내용을 알지 못하게 되어 게임 내에서 헤매게 됩니다. 따라서 성급한 유저를 위하여 빨리 진행하되 최소한의 내용은 숙지할 수 있을 정도의 시간 동안은 내용을 보여주어야 합니다.

1. 한 줄의 대화를 한 글자씩 유저에게 보여주다가 유저가 스킵하면 남은 글자를 전부 보여준다..
1. 모든 글자를 보여준 뒤 잠시 기다린다.
1. 충분히 기다렸거나 기다리던 도중 유저가 스킵하면, 다시 다음 줄의 대화를 한 글자씩 보여주기 시작한다.

이를 위의 코드에 추가하면 다음과 같이 됩니다.

```csharp
enum State
{
    Playing,
    PlayingSkipping,
}

IEnumerator Run()
{
    for (int i = 0; i < texts.Count; i += 1)
    {
        yield return PlayLine(texts[i]);
    }
}

IEnumerator PlayLine(string text)
{
    for (int i = 0; i < text.Length() + 1; i += 1)
    {
        if (state == State.PlayingSkipping)
        {
            uiText.text = text;
            state = State.Playing;
            break;
        }
        yield return new WaitForSeconds(0.05f);
        uiText.text = text.Substring(0, i);
    }

    yield return new WaitForSeconds(0.5f);

    for (int i=0; i<25; i+=1)
    {
        yield return new WaitForSeconds(0.1f);
        if (state == State.PlayingSkipping)
        {
            state = State.Playing;
            break;
        }
    }
}

public void Skip()
{
    state = State.PlayingSkipping;
}
```

위 코드의 특이한 점은 유저가 skip을 눌렀을 때 단순히 state변수만을 수정한다는 점입니다. 이렇게 함으로써 유저가 여러 번 스킵을 불러도 안전하며, 코드의 실행 흐름이 `PlayLine`함수 안에 밀집되기 때문에 코드를 관리하기도 쉽습니다.

완성된 예시 프로젝트는 아래 [깃헙 링크](https://github.com/majecty/DialogueByCoroutineExample)에서 볼 수 있습니다.

참고.

이렇게 만든 대화 시스템은 유니티 UI에서 지원하는 RichText를 잘 지원하지 못합니다. RichText를 쉽게 쓸 수 있도록 제가 만든 간단한 라이브러리가 있습니다. 이 라이브러리를 쓰면 두 줄을 바꾸는 것으로 RichText를 지원할 수 있습니다. [이 링크](https://github.com/majecty/Unity3dRichTextHelper)에서 확인하세요.

```csharp
for (int i = 0; i < text.RichTextLength() + 1; i += 1)
{
    yield return new WaitForSeconds(0.05f);
    uiText.text = text.RichTextSubString(i);
}
```