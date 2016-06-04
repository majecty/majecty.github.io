git branch -f master
stack run site clean
stack run site build
cp -a _site/. .
git checkout master
git add .
git commit -m 'Publish'
git push origin develop
git push origin master -f
git checkout develop
