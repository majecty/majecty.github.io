#!/bin/sh
# Zola 발행: https://www.getzola.org/documentation/getting-started/installation/ 에서 zola 설치 필요
set -e
git branch -f main
zola build
cp -a public/. .
git checkout main
git add .
git commit -m 'Publish'
git push origin develop
git push origin main -f
git checkout develop
