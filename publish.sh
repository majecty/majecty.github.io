git branch -f main
stack run site clean
stack run site build
cp -a _site/. .
git checkout main
git add .
git commit -m 'Publish'
git push origin develop
git push origin main -f
git checkout develop
