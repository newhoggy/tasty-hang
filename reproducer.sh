#!/usr/bin/env bash

set -euo pipefail

run_hang() {
  cabal run tasty-hang-bad
}

# each invocation is doing a lot of IO, so you can set it to 1.5 * number of CPUs
n_loops=48

trap "echo 'Killing all loops...'; kill 0" SIGINT SIGTERM

cabal clean
cabal build all

mkdir -p logs

for i in $(seq 1 $n_loops); do
  (
    date >&2
    for x in {0..99999}; do
      echo "== $x =="
      echo "== $x ==" >&2
      run_hang
    done
  ) > ./logs/stdout-$i.log 2> ./logs/stderr-$i.log &
done

wait

echo "DONE!"
