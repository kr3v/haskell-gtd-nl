#!/usr/bin/env bash

set -x

t0=$(date --iso-8601=seconds)
root="reports/$t0"

mkdir -p "$root"

run_benchmark() {
    t=$(date --iso-8601=seconds)
    rts=$(echo "$1" | sed 's/[ -]/_/g')
    report="$root/$rts.$t.txt"
    killall -9 haskell-gtd-nl-parser haskell-gtd-nl-server
    echo "$1" >> "$report"
    cabal v2-bench haskell-gtd-nl-bench \
        --benchmark-options="+RTS $1 -RTS bench --criterion-report-path $root/$rts.$t.json --report-path $report $2" \
        >> "$root/bench.$1.$t.stdout" \
        2>> "$root/bench.$1.$t.stderr"
}

run_benchmark_with_all_parameters() {
    run_benchmark "-N1 -A1M"   "$1"
    run_benchmark "-N1 -A4M"   "$1"
    run_benchmark "-N1 -A32M"  "$1"
    run_benchmark "-N1 -A128M" "$1"
    run_benchmark "-N1 -A512M" "$1"

    run_benchmark "-N8 -A1M"   "$1"
    run_benchmark "-N8 -A4M"   "$1"
    run_benchmark "-N8 -A32M"  "$1"
    run_benchmark "-N8 -A128M" "$1"
    run_benchmark "-N8 -A512M" "$1"

    run_benchmark "-N24 -A1M"   "$1"
    run_benchmark "-N24 -A4M"   "$1"
    run_benchmark "-N24 -A32M"  "$1"
    run_benchmark "-N24 -A128M" "$1"
    run_benchmark "-N24 -A512M" "$1"
}

run_benchmark_with_all_parameters "--repo-path /data/workspace/data/workspace/workspace/repos/cloned-public/plutus --des-type Library --des-name marlowe-internal"
