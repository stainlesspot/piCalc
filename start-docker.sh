#!/bin/bash

docker run \
  --name picalc \
  --mount "type=bind,source=$(pwd)/stats,target=/piCalc/stats-bound" \
  --rm \
  picalc \
  ./stats-bound/gather.sh 5 32
