#!/bin/bash

num_runs=${1:-5}
thread_limit=${2:-32}

precision=50000
granularities=(1 2 4 8 16 32 64 128)
all_threads=(1 2 4 8 12 16 20 24 28 32)

if [ $thread_limit -le 4 ]; then
  all_threads=(1 2 3 4)
fi
threads=()
for t in "${all_threads[@]}"; do
  if [ $t -le $thread_limit ]; then
    threads+=($t)
  fi
done

cd $(dirname $0)/..
stack build

outputFile=stats/stats-${precision}.csv
if [ -f $outputFile ]; then
  mv $outputFile ${outputFile}.old
fi
echo 'runNo,numThreads,granularity,time' > $outputFile

for (( r = 0; r < $num_runs; ++r )); do
  for t in "${threads[@]}"; do
    for g in "${granularities[@]}"; do
      cmd="stack run -- -q -o _  -p $precision -t $t -g $g"
      echo -ne "running '$cmd' (run $r)\r"
      time="$($cmd)"
      echo "$r,$t,$g,$time" >> $outputFile
    done
  done
done
