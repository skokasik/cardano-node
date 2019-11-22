#!/usr/bin/env bash

set -e

RUNNER=${RUNNER:-cabal new-run --}

 ARGS=(  real-protocol
         --database-path           "./db"
         --genesis-file            "configuration/mainnet-genesis.json"
         --topology                "configuration/topology-proxy-follower.json"
         --socket-path              "./socket/singlenode"
         --config                  "./configuration/mainnet-proxy-follower.yaml"
         --port                    7776
 )

${RUNNER} exe:cardano-node "${ARGS[@]}"
