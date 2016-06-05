---
title: function을 fold하기
author: 주형
tags: haskell, foldr, 하스켈
---

foldr은 리스트에서 값을 모아가는 함수입니다. 하스켈에서는 함수가 다른 값처럼 인자로 혹은, 리턴값으로 사용될 수 있죠. foldr에서도 리스트를 돌면서 함수를 만들어나갈 수 있습니다.

리스트에 있는 함수를들 전부 compose하는 함수를 생각해 봅시다.
```Haskell
compose :: [x -> x] -> (x -> x)
compose = foldr (.) id

-- 예시)
compose [ (+1), (+2), (+3) ]
(+1) . ( (+2) . ( (+3) . id) )
(+1) . ( (+2) . (+3) )
(+1) . (+5)
(+6)
```

좀 더 응용하면 값을 왼쪽부터 더해가는 suml을 foldr로 만들 수 있습니다.
foldr

# 참고

[http://www.cs.nott.ac.uk/~pszgmh/fold.pdf](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
