---
title: shared library를 어떻게 찾는가
author: 주형
tags: dynamic link
---

# Shared Library란

static library와 대응되는 말. static library는 링크할 때 라이브러리가 같이 포함된다. shared library는 링크할 때 포함되지 않고 실행할 때 라이브러리를 불러온다.

## 왜 Shared Library를 쓰는가

* 라이브러리가 빌드될 때 포함되지 않기 때문에 빌드 결과물의 용량이 줄어든다.
* 다시 빌드하지 않고 라이브러리 업데이트가 가능하다.

## Sahred Library의 일반적인 사용법

/lib 이나 /usr/local/lib이나 /usr/lib 같은 곳에 라이브러리를 설치한다. shared library를 필요로하는 프로그램을 실행시킬 때 이 표준 라이브러리에서 사용한다.

# 문제가 생기는 지점

* /lib이나 /usr/lib같은 곳에 접근할 수 있는 권한이 없을 때 원하는 라이브러리를 설치할 수 없다.
* 정식버전이 아닌 라이브러리를 테스트하고 싶을 때 굳이 불안정한 버전을 설치하고 싶지 않다.
* 빌드 결과물을 shared library와 함께 배포하고 싶을 때

# 문제 해결하기

## LD_LIBRARY_PATH

환경변수 LD_LIBRARY_PATH 를 사용하면 표준적인 위치에서 찾기 전에 shared library를 탐색할 위치를 지정할 수 있다.

## Linux's RPATH

Linux에서 RPATH를 이용하면 링크 타임에 라이브러리 로드 패쓰를 상대 위치로 지정할 수 있다.

## OS X's @rpath

OS X 에서는 shared library의 install name에 @rpath를 사용하는 걸 권장한다. install name에 @rpath가 들어있으면 그 라이브러리를 사용하는 실행파일 혹은 라이브러리의 헤더에 적혀있는 rpath를 사용하여 shared library를 찾을 수 있다. rpath는 라이브러리를 링크할 때 설정할 수 있다. 혹은 빌드가 끝난 결과물의 rpath를 설정할 수 있다.

otool -l (빌드된 파일) 을 사용하면 rpath가 어떻게 설정되어있는 지 볼 수 있다.

otool -l output 예시. rpath가 상대좌표 lib으로 세팅되어있으면 다음과 같은 결과를 볼 수 있다.
```
Load command 16
          cmd LC_RPATH
      cmdsize 16
         path lib (offset 12)
```

`install_name_tool -add_rpath (rpath_directory) (binary)` 를 사용하면 바이너리 파일에 rpath를 추가할 수 있다.

# Reference

* [linux에서 shared library를 찾아서 로드하는 ld.so에 대한 내용](http://man7.org/linux/man-pages/man8/ld.so.8.html)

* [Linux에서 shared library에 대한 자세한 내용](http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html)

* [왜 LD_LIBRARY_PATH는 나쁜가](http://xahlee.info/UnixResource_dir/_/ldpath.html) : LD_LIBRARY_PATH를 어떻게 쓰면 좋은 지에 대해서 잘 설명되어있다.

* [apple developer dynamiclibrary](https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/OverviewOfDynamicLibraries.html#//apple_ref/doc/uid/TP40001873-SW2)

* [OS X에서 어떤 순서로 Path를 찾는가](http://blog.leshill.org/blog/2010/04/24/dynamic-load-paths-in-osx.html)

* [OSX reference for DYLD_LIBRARY_PATH](https://developer.apple.com/legacy/library/documentation/Darwin/Reference/ManPages/man1/dyld.1.html)

* [Debian에서 Rpath처리하기](https://wiki.debian.org/RpathIssue)

* [OS x rpath 사용 예시](https://github.com/conda/conda-build/issues/279)
