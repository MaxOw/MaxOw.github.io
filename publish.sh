
export REMOTE=$(git config remote.origin.url | sed 's/.*:\/\///')
git clone https://${REMOTE}
cp _site/* maxow.github.io/ -r
cd maxow.github.io
git config --global user.email "$GIT_EMAIL"
git config --global user.name  "$GIT_NAME"
git remote add github https://${GH_TOKEN}@${REMOTE}
git add --all
git status
git commit -m "Built by Travis ( build nr $TRAVIS_BUILD_NUMBER )"
git push github master:master | grep -v http
