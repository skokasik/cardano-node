#!/usr/bin/env bash

TARGETDIR=cardano-byron-proxy.git
if [ ! -d ${TARGETDIR} ]; then
  echo "cannot find byron-proxy. run './prepare-byron-proxy.sh' first."
  exit 1
fi

BASEDIR=$(realpath $(dirname $0))
set -eo pipefail

cd ${TARGETDIR}

if [[ $1 == 'stack' ]]; then
  PROXY="stack --nix exec cardano-byron-proxy -- "
  shift
elif [[ $1 == 'cabal' ]]; then
  PROXY="cabal v2-run cardano-byron-proxy -- "
  shift
else
  # Default to stack
  PROXY="stack --nix exec cardano-byron-proxy -- "
fi

${PROXY} \
  +RTS -T -RTS \
  --database-path state-proxy-mainnet/db \
  --index-path state-proxy-mainnet/index \
  --configuration-file ./configuration/configuration.yaml \
  --configuration-key mainnet_full \
  --topology ${BASEDIR}/configuration/topology-byron-proxy.yaml \
  --logger-config ${BASEDIR}/configuration/log-config-byron-proxy.yaml \
  --local-addr [127.0.0.1]:7777 \
   \
   \
   \
   \
   $@

cd ${BASEDIR}
