#!/bin/bash
#This script takes as argument the name of your ssh key. By default it is using id_rsa.

set -o errexit -o pipefail

eval "$(ssh-agent)"

if [ -z "$1" ]
  then
    SSH_KEY_NAME=id_rsa
  else
    SSH_KEY_NAME=$1
fi

ssh-add "$HOME/.ssh/$SSH_KEY_NAME"

DOCKER_BUILDKIT=1 docker build \
  --ssh default \
  -f Dockerfile.dependency \
  -t game-randomizer-dependency \
  .

docker build \
  -f Dockerfile.development \
  -t game-randomizer-dev \
  .

docker-compose up -d

docker exec game-randomizer-erl rebar3 compile
