---
title: 다이나믹 라이브러리를 어떻게 찾는가
author: 주형
tags: dynamic link
---

# linux에서

## 기본으로 찾는 방법

1. /etc/ld.so.cache를 검색합니다. 이 캐시는 ldconfig를 사용하면 다시 만들어집니다.
2. cache에서 찾이 못하면 /lib이나 /usr/lib에서 찾습니다.

## 덮어 쓰기

LD_LIBRARY_PATH 환경변수를 세팅하면 캐시를 찾기 전에 환경변수의 디렉토리를 찾아봅니다.


# OS X에서

OS X에서도 linux와 마찬가지로 환경 편수를 통해서 기본값을 덮어 씌울 수 있습니다.

* DYLD_LIBRARY_PATH : LD_LIBRARY_PATH 처럼 먼저 적용되서 기본 값을 덮어 씌웁니다.
* DYLD_FALLBACK_LIBRARY_PATH : LD_LIBRARY_PATH에서도 못찾고, 기본 검색에서도 못 찾은 경우 마지막으로 찾아보는 PATH

## LD_LIBRARY_PATH

문서에서는 DYLD_LIBRARY_PATH를 사용하라고 되어있지만 LD_LIBRARY_PATH를 사용해도 동작합니다.(이게 왜 동작하는 지는 reference 못 찾음)


# 헤더들의 권장하는 사용법

LD_LIBRARY_PATH나DYLD_LIBRARY_PATH는 일반적인 상황에서 사용하지 않습니다. 개발이나 테스트 용도로 잠시 값을 덮어씌울 때 사용합니다.

# Reference

* [linux에서 shared library를 찾아서 로드하는 ld.so에 대한 내용](http://man7.org/linux/man-pages/man8/ld.so.8.html)

* [Linux에서 shared library에 대한 자세한 내용](http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html)

* [왜 LD_LIBRARY_PATH는 나쁜가](http://xahlee.info/UnixResource_dir/_/ldpath.html) : LD_LIBRARY_PATH를 어떻게 쓰면 좋은 지에 대해서 잘 설명되어있다.

* [OS X에서 어떤 순서로 Path를 찾는가](http://blog.leshill.org/blog/2010/04/24/dynamic-load-paths-in-osx.html)

* [OSX reference for DYLD_LIBRARY_PATH](https://developer.apple.com/legacy/library/documentation/Darwin/Reference/ManPages/man1/dyld.1.html)
