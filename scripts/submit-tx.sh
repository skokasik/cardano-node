#!/usr/bin/env bash

test -z "$1" -o ! -f "$1" -o ! -r "$1" && {
        cat >&1 <<EOF
Usage:  $(basename $0) TX-FILE
EOF
        exit 1
}
TX="$1"
shift

#CMD="stack exec --nix cardano-node -- "
CMD="cabal v2-run"

. $(dirname $0)/lib-node.sh

ALGO="real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        submit-tx
        --tx           "$TX"
        --node-id      "0"
        --${ALGO}
        --genesis-file "${genesis_file}"
        --genesis-hash "${genesis_hash}"
        --socket-dir   "./socket/"
        --topology     "configuration/simple-topology.json"
)


set -x
${CMD} cardano-cli -- ${NETARGS[*]} "$@"
