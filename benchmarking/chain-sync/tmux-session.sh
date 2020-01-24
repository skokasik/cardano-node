#!/bin/sh

set -x
tmux new-window -n ByronProxy "./launch_mainnet_proxy.sh $1; $SHELL"

tmux new-window -n Benchmark "./benchmark-chain-sync-mainnet.sh $1; $SHELL"

