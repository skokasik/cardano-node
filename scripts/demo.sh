#!/usr/bin/env bash

#tmux new-session -s 'Demo' -t demo

ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS="--system-start \"${NOW}\" --slot-duration 4 node -t configuration/simple-topology.json ${ALGO}"
#SCR="./scripts/start-node.sh"
CMD="stack exec cardano-node --"
HOST="127.0.0.1"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}

rm -rf db-0 db-1 db-2
tmux split-window -v
tmux split-window -v
tmux split-window -v
tmux select-layout even-vertical

tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) ${NETARGS} -n 0 --host ${HOST} --port 3000" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $(mklogcfg 1) ${NETARGS} -n 1 --host ${HOST} --port 3001" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) ${NETARGS} -n 2 --host ${HOST} --port 3002" C-m
tmux select-pane -t 3
tmux send-keys "./scripts/submit-tx.sh -n 0 --real-pbft --address a --amount 4000 --txin abababab --txix 0"
