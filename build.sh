#!/bin/bash

set -e
TAG=${1:-latest}
BUILD=`mktemp -d`
echo "Building in $BUILD"

git clone . $BUILD
cd $BUILD
# cp Dockerfile marv.sh $BUILD
tar zcf marv.tgz alpha/ command.rkt core/ drivers/ examples/ info.rkt main.rkt log.rkt utils/
docker build -t marvlogic/marv:$TAG $BUILD
rm -rf $BUILD