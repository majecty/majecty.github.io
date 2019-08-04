---
title: PowerShell 시작하기
author: 주형
tags: powershell, windows, 파워셀
---

# Profile

bashrc나 zshrc처럼 PowerShell 세션이 시작할 때 실행되는 파일. `write $profile` 명령어로 해당 파일을 찾을 수 있다.

```powershell
write $profile
```

## Emacs 모드 설정


`PSReadLine`을 설정하여, 리눅스에서 쉘 작업할 때처럼 `C-a`, `C-e`, `C-r` 를 사용할 수 있다.

```powershell
Set-PSReadLineOption -EditMode Emacs
```

# 도움말

알고싶은 cmdlet에 대해 `help` cmdlet를 사용하여 정보를 얻는다.

```help Get-ChildItem```

알고싶은 cmdlet의 부분만 검색할 수 있다.

```help child```

`-Examples` 옵션이나 `-Category` 옵션을 주어 원하는 부분만 볼 수 있다.

```powershell
help Where-Object -Examples
help Where-Object -Category Alias
```

## Alias

내가 알고있는 alias의 원본 cmdlet을 알고 싶을 때

```Get-Alias -Name cd```

내가 알고있는 명령어의 alias를 알고 싶을 때

```Get-Alias -Definition Set-Location```


# PowerShell 변수

변수의 할당

```powershell
$x = 3
$y = Get-Location
```

변수의 출력

```powershell
$x
$y
```

변수의 함수 호출

```powershell
$x.Equals($y)
$y.Path
```

오브젝트의 프로퍼티/메쏘드들 출력

```powershell
Get-Member -InputObject $x
```

# PowerShell 파이프

PowerShell은 unix의 Shell과는 다르게 Object를 파이프를 통해 건넨다.

```powershell
$x | write
$x | Get-Member
```

## ForEach-Object

`ForEach-Object`를 사용하여 파이프로 받은 오브젝트(들)에 대해 원하는 명령을 실행할 수 있다.

```powershell
Get-Location | ForEach-Object path
```

`ForEach-Object`는 %로 대체할 수 있다.

```powershell
Get-Location | % path
```

`ForEach-Object`는 인자로 중괄호로 감싸여진 Script Block을 받을 수 있다. 이 때 $_ 변수가 각 엘리먼트이다.

```powershell
30000, 56798, 12432 | ForEach-Object {$_/1024}
```

## Where-Object

`Where-Object`를 사용하여 리스트의 원하는 것만 골라낼 수 있다.

```powershell
1,2,3 | Where-Object { $_ % 2 -eq 0 }
```

`Where-Object`는 `?` 로 alias되어있다.

```powershell
1,2,3 | ? { $_ % 2 -eq 0 }
```


# 참고자료

* [About Profiles - docs.microsoft.com](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_profiles?view=powershell-6)
* [Pipeline - docs.microsoft.com](https://docs.microsoft.com/ko-kr/powershell/scripting/learn/understanding-the-powershell-pipeline?view=powershell-6)
* [ForEach-Object - docs.microsoft.com](https://docs.microsoft.com/ko-kr/powershell/module/Microsoft.PowerShell.Core/ForEach-Object?view=powershell-5.0)
* [Where-Object - docs.microsoft.com](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/where-object?view=powershell-6)