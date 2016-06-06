---
title: function을 fold하기
author: 주형
tags: haskell, foldr, 하스켈
description: fundtion을 fold해보자
---

foldr은 리스트에서 값을 모아가는 함수입니다. 하스켈에서는 함수가 다른 값처럼 인자로 혹은, 리턴값으로 사용될 수 있죠. foldr에서도 리스트를 돌면서 함수를 만들어나갈 수 있습니다.

리스트에 있는 함수를들 전부 compose하는 함수를 생각해 봅시다.
```Haskell
compose :: [x -> x] -> (x -> x)
compose = foldr (.) id

-- 예시)
-- compose [ (+1), (+2), (+3) ]
-- -> (+1) . ( (+2) . ( (+3) . id) )
-- -> (+1) . ( (+2) . (+3) )
-- -> (+1) . (+5)
-- -> (+6)
```

좀 더 응용하면 값을 왼쪽부터 더해가는 suml을 foldr로 만들 수 있습니다.

```Haskell
suml :: [Int] -> Int
suml xs = suml' xs 0
    where
        suml' [] n = n
        suml' (x:xs) n = suml' xs (n + x)
```

f와 v를 찾으면 foldr로 suml'을 만들어낼 수 있습니다.

```Haskell
suml' [] = v
suml' (x:xs) = f x (suml' xs)
```

v 찾기
```Haskell
suml' [] n = v n
-- -> suml' [] n = n = v n
-- -> v = id
```

f 찾기
```Haskell
suml' (x:xs) n = f x (suml' xs) n
-> suml' xs (n + x) = f x (suml' xs) n
-- suml' xs를 y로 치환
-> y (n + x) = f x y n
-> f = \x y n -> y (n + x)
-- 인자를 2개로 맞춥니다.
-> f = \x y -> (\n -> y (n + x))

suml' = foldr (\x y -> (\n -> y (n + x))) id
suml xs = foldr (\x y -> (\n -> y (n + x))) id xs 0
```

마찬가지로 foldl 역시 foldr을 사용해서 만드어낼 수 있습니다.

```Haskell
-- suml'에서 +를 f를 사용합니다.
foldl xs v = foldr (\x y -> (\n -> y (f n x))) id xs v
```

# 참고

[http://www.cs.nott.ac.uk/~pszgmh/fold.pdf](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
