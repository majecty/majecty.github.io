---
title: foldr과 foldl
author: 주형
tags: haskell, foldr, 하스켈, 2016-06-07-foldr-presentation
home: haskell
description: foldr과 foldl은 어떻게 다른가
---

# 왼쪽과 오른쪽
foldr의 r은 right입니다. 리스트의 오른쪽 원소부터 값을 쌓아나갑니다. 1부터 3까지 더하는 foldr함수를 전개해 봅시다.

```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr (+) 0 [1,2,3]
1 + (foldr (+) 0 [2,3])
1 + (2 + (foldr (+) 0 [3]))
1 + (2 + (3 + (foldr (+) 0 [])))
1 + (2 + (3 + (0)))
```

괄호 안부터 계산되므로 기본 값 0에 3, 2, 1 차례대로 값을 더해갑니다.

반대로 값을 왼쪽부터 계산해가는 foldl이 있습니다.

```Haskell
foldl :: (b -> a -> b) -> b -> [a] -> b

foldl (+) 0 [1,2,3]
foldl (+) (0 + 1) [2,3]
foldl (+) ((0 + 1) + 2) [3]
foldl (+) ((0 + 1) + 2 + 3) []
((0 + 1) + 2 + 3)
```

# strict한 foldl'

foldr과 foldl에는 연산 순서 말고도 다른 점이 있습니다.

foldr은 모든 리스트가 풀어진 다음에서야 값을 하나씩 합쳐나갈 수 있습니다. 반면에 foldl의 경우에는 그때 그때 값을 합쳐나갈 수 있습니다.

```Haskell
foldl :: (b -> a -> b) -> b -> [a] -> b

foldl (+) 0 [1,2,3]
foldl (+) (0 + 1) [2,3]
foldl (+) (1 + 2) [3]
foldl (+) (3 + 3) []
6
```

이렇게 값을 합쳐나가게 되면 foldr에 비해서 메모리를 많이 절약할 수 있습니다. 하지만 하스켈의 foldl은 lazy하게 계산하기 때문에 이런 메모리 절약효과를 얻을 수 없습니다. 대신 foldl의 strict version인 foldl' 함수를 사용하면 메모리 절약효과를 얻을 수 있습니다.

# short-circuit이 가능한 foldr

foldr 은 연산이 중간까지만 필요한 경우 뒤의 연산을 하지 않습니다.

리스트의 앞에서부터 0이 나오기 전 까지의 원소만 얻고 싶은 함수를 구현해봅시다. 리스트의 원소 중에서 0 이후에 나오는 원소에 대해서는 루프를 돌 필요가 없습니다.

```Haskell
consUntilZero a xs = if a == 0 then [] else a:xs

foldr consUntilZero 0 [1,2,0,3,4,5]

consUntilZero 1 (foldr consUntilZero 0 [2,0,3])
1:(foldr consUntilZero 0 [2,0,3,4,5])
1:(2:(foldr consUntilZero 0 [0,3,4,5]))
1:(2:(consUntilZero 0 (foldr consUntilZero 0 [3,4,5]))
1:(2:[])
[1,2]
```

# 결론

연산이 중간에 끝나는 경우에는 foldr을 쓰는 것이 좋습니다. 연산을 해야하는 데이터가 크고 전체를 돌아야한다면 foldl'을 쓰는 것이 좋습니다.

# 참고
[https://wiki.haskell.org/Foldr_Foldl_Foldl'](https://wiki.haskell.org/Foldr_Foldl_Foldl')
