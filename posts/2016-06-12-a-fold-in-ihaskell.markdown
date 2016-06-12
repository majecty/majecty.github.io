---
title: iHaskell의 fold사용 예
author: 주형
tags: haskell, foldr, 하스켈
home: haskell
description: iHaskell 소스코드에서 찾아본 foldl의 사용 예시
---

iHaskell 의 소스코드를 읽다가 foldl'을 깔끔하게 사용한 코드를 찾아서 정리합니다.

iHaskell에서 타입을 출력할 때 출력해도 의미 없는 module의 이름들을 지워서 출력합니다. 예를 들면 GHC.Types.Int 라는 타입 이름이 있으면 앞의 GHC.Types를 지우고 Int만 프린트합니다. typeCleaner함수가 전체 타입 이름에서 의미가 없는 모듈 이름을 지우는 역할을 합니다. 의미 없는 모듈 이름들은 ignoreTypePrefixes에 정의되어있습니다.
```Haskell
-- src/IHaskell/Eval/Evaluate.hs#L102
ignoreTypePrefixes :: [String]
ignoreTypePrefixes = [ "GHC.Types"
                     , "GHC.Base"
                     , "GHC.Show"
                     , "System.IO"
                     , "GHC.Float"
                     , ":Interactive"
                     , "GHC.Num"
                     , "GHC.IO"
                     , "GHC.Integer.Type"
                     ]

typeCleaner :: String -> String
typeCleaner = useStringType . foldl' (.) id (map (`replace` "") fullPrefixes)
  where
    fullPrefixes = map (++ ".") ignoreTypePrefixes
    useStringType = replace "[Char]" "String"
```

위의 내용이 iHaskell 소스코드에 있는 내용입니다.

fullPrefixes를 풀어써보면 다음과 같습니다.

```Haskell

fullPrefix :: [String]
fullPrefix = [ "GHC.Types."
             , "GHC.Base."
             , "GHC.Show."
             , "System.IO."
             , "GHC.Float."
             , ":Interactive."
             , "GHC.Num."
             , "GHC.IO."
             , "GHC.Integer.Type."
             ]
```

`map (`replace` "") fullPrefixes` 부분을 풀어서 써보면 다음과 같습니다.

```Haskell
map (`replace` "") fullPrefixes :: String -> String
-> [ replace "GHC.Types." ""
   , replace "GHC.Show." ""
   , replace "System.IO." ""
   , replace "GHC.Float." ""
   , replace ":Interactive." ""
   , replace "GHC.Num." ""
   , replace "GHC.IO." ""
   , replace "GHC.Integer.Type." ""
   ] 
```

각각 타입 이름을 받아서 모듈 이름을 하나씩 빈 스트링으로 치환해주는 함수들입니다.
이 리스트를 foldl' (.) id 로 하나로 합치면 스트링을 받아서 불필요한 모든 모듈 이름을 없애주는 하나의 함수가 됩니다.

# 참고
[https://github.com/gibiansky/IHaskell/blob/v0.8.3.0/src/IHaskell/Eval/Evaluate.hs#L115](https://github.com/gibiansky/IHaskell/blob/v0.8.3.0/src/IHaskell/Eval/Evaluate.hs#L115)
