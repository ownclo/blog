#!/bin/bash

check_environment() {
    local actual="$1"
    local expected="$2"
    local message="$3"

    if [[ $actual != $expected ]]; then
      echo "DEPLOYMENT SKIPPED (${message})"
      exit 0
    fi
}


check_environment "$TRAVIS_REPO_SLUG" "lunaryorn/blog" "not our repo"
check_environment "$TRAVIS_BRANCH" "master" "not the master branch"
check_environment "$TRAVIS_PULL_REQUEST" "false" "pull request"
check_environment "$TRAVIS_SECURE_ENV_VARS" "true" "secure variables missing"

echo "Publishing blog... \n"

# Git setup
export GIT_COMMITTER_EMAIL='lunaryorn@gmail.com'
export GIT_COMMITTER_NAME='Sebastian Wiesner'
export GIT_AUTHOR_EMAIL='travis@travis-ci.org'
export GIT_AUTHOR_NAME='Travis CI'

git clone --quiet --branch=master https://${GH_TOKEN}@github.com/lunaryorn/lunaryorn.github.io.git deploy
rsync -a --delete --exclude /.git/ _site/ deploy

cd deploy
git add --force --all .
git commit -m "Update from lunaryorn/blog@${TRAVIS_COMMIT}"
git push --force --quiet origin master

echo "Published blog!"
