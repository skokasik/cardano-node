#!/usr/bin/env bash

set -e

RUNNER=${RUNNER:-cabal new-run --}
TOPOLOGY=${TOPOLOGY:-"configuration/topology-proxy-follower.json"}

 ARGS=(  real-protocol
         --database-path           "./db"
         --genesis-file            "configuration/mainnet-genesis.json"
         --topology                "${TOPOLOGY}"
         --socket-dir              "./socket/singlenode"
         --config                  "./configuration/mainnet-proxy-follower.yaml"
         --port                    7776
 )

${RUNNER} exe:cardano-node "${ARGS[@]}"
