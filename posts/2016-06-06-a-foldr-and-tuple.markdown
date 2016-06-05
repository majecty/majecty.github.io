---
title: foldr과 tuple
author: 주형
tags: haskell, foldr, 하스켈
---

tuple을 사용하면 foldr로 좀 더 많은 일들을 할 수 있습니다.

# dropWhile 을 foldr로 표현하기

```Haskell
y = foldr f v

y [] = v
y (x:xs) = x `f` (y xs)

dropWhile :: (a -> Bool) -> ([a] -> [a])
dropWhile p [] = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x : xs

-- foldr 스타일로 바꾸어 봅시다.

dropWhile p (x:xs) = (\y ys -> if p y then dropWhile p ys else y : xs) x (dropWhile p xs)

-- foldr 스타일과 비슷하게 되었지만, 함수 안에서 xs가 필요해서 foldr로 바꿀 수 없습니다.
```

foldr 의 f 함수는 foldr이 적용된 리스트를 받아와야하지만 dropWhile의 f함수는 foldr이 적용되지 않은 리스트가 필요해서 foldr로 바꿀 수 없습니다.

하지만 직접 dropWhile을 foldr로 만드는 게 아니라 한 번 돌아가는 과정을 거치면 foldr을 사용해서 dropWhile을 만들어낼 수 있습니다.

```Haskell
dropWhile' :: (a -> Bool) -> ([a] -> ([a], [a]))
dropWhile' p xs = (dropWhile p xs, xs)

dropWhile' p [] = ([], [])
dropWhile' p (x:xs) = if p x then (dropWhile p xs, x:xs) else (x:xs, x:xs)
dropWhile' p (x:xs) =
    let (zs, _) = dropWhile' p xs
    in if p x then (zs, x:xs) else (x:xs, x:xs)
-- zs = dropWhile p xs 
dropWhile' p (x:xs) = (\y (zs, ys) -> if p y then (zs, y:ys) else (y:ys, y:ys)) x (dropWhile' p xs)
```

foldr만 사용했을 때는 foldr의 f함수가 현재 element, 남은 리스트에 대해서 fold된 값 이 2가지 값만 사용할 수 있었습니다. foldr과 tuple을 같이 사용하면, 현재 element, 남은 리스트에 대해서 fold된 값, 남은 리스트 이렇게 세 값을 사용할 수 있습니다. 더 많은 종류의 recursive 함수들을 foldr을 사용하여 구현할 수 있게 됩니다.

# 참고

[http://www.cs.nott.ac.uk/~pszgmh/fold.pdf](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
