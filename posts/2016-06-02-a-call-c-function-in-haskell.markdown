---
title: 하스켈에서 C 함수 호출하기
author: 주형
tags: haskell, 하스켈
home: haskell
---

하스켈에서 간단하게 c 함수를 호출할 수 있습니다.
다음과 같이 C 함수에 대한 타입을 정의해주면 바로 호출할 수 있습니다.

```Haskell
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.String

foreign import ccall "printf" printf :: CString -> IO ()

main :: IO ()
main = do
  cstr <- newCString "Hello c printf\n"
  printf cstr
```

특정 헤더 파일에 있는 함수를 쓰는 경우에는 함수 앞에 헤더파일을 명시해주면 됩니다.

```Haskell
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types

foreign import ccall "math.h exp" c_exp :: CDouble -> CDouble

main :: IO ()
main = do
  putStrLn $ show (c_exp 1)
  putStrLn $ show (c_exp 2)
```

다음 글에서 C함수의 포인터를 하스켈에서 다루는 법, C의 structure 를 다루는 법 등을 다뤄보겠습니다.
