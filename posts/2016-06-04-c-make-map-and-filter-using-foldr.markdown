---
title: foldr로 map과 filter만들기
author: 주형
tags: haskell, foldr
home: haskell
---

리스트를 다루는 함수로 map, filter, fold를 자주 사용합니다. foldr을 사용해서 다른 두 함수를 만들어낼 수 있습니다. 하지만 다른 두 함수로는 foldr을 만들어낼 수 없습니다. foldr을 사용해서 어떻게 map이나 filter를 만들어 내는 지 살펴보겠습니다.

# foldr 다시 보기
```Haskell
-- f와 v를 찾아내는 것이 목적입니다.
y = foldr f v

-- map과 filter를 이 꼴로 만들어내면 됩니다.
y [] = v
y (x:xs) = x `f` (y xs)
```

# map을 fold로 구현하기
```Haskell
map :: (a -> b) -> [a] -> [b]

map f [] = []
map f (x:xs) = f x : (map f xs)

-- 다음과 같이 변형시켜보겠습니다.
map f (x:xs) = (:) (f x) (map f xs)
map f (x:xs) = ((:) . f) x (map f xs)

-- foldr의 패턴으로 만들어졌습니다.
map = foldr ((:) . f) []
```

# filter을 fold로 구현하기
```Haskell
filter :: (a -> Bool) -> [a] -> [b]

filter predicate [] = []
filter predicate (x:xs) = if predicate x then x:(filter predicate xs) else (filter predicate xs)

-- 위 함수를 foldr 형식으로 바꿔보겠습니다.
filter predicate (x:xs) = (++) (if predicate x then [x] else [])  (filter predicate xs)
filter predicate (x:xs) = ((++) . (\x -> if predicate x then [x] else [])) x (filter predicate xs)

-- 따라서 
filter predicate = foldr ((++) . (\x -> if predicate x then [x] else [])) []
```

