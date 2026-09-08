# Blog (Zola)

_배포하기_

develop브랜치에서 작업을 커밋한 뒤 `./publish.sh` 를 실행
(`zola` 설치 필요: https://www.getzola.org/documentation/getting-started/installation/)

_로컬 테스트하기_

`zola serve` 후 http://127.0.0.1:1111 접속

_빌드하기_

`zola build` (결과물은 `public/`, git 무시됨)

_새 글 쓰기_

`content/posts/YYYY-MM-DD-slug.md` 파일을 만들고 frontmatter를 채운다:

```toml
+++
title = "제목"
slug = "YYYY-MM-DD-slug"
date = 2026-01-01
template = "post.html"
aliases = ["posts/YYYY-MM-DD-slug.html"]
[taxonomies]
tags = ["haskell"]
[extra]
summary = "요약"
+++
```

`aliases`는 구 `.html` 주소를 리다이렉트로 보존한다. 태그 구주소 스텁은
빌드 후 `/tmp/pi/tag-stubs.ts` 흐름으로 재생성한다.
