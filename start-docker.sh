#!/bin/bash

docker run \
  --name picalc \
  --mount "type=bind,source=$(pwd)/stat,target=/piCalc/stat-bound" \
  picalc \
  ./stat-bound/gather.sh 5 32
