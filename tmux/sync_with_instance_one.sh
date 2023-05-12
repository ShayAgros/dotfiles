#!/usr/bin/env bash


# Quick and dirty script to sync aws-c-io content with pane 1

cd ~/workspace/amazon_repos/crt/aws-c-io

tmux split-window -d -p 20 -f "~/workspace/scripts/send_changed_files.sh -i 1 || { tmux display-message \"Sending to ssh #${ssh_ixs} failed\" ; sleep 2 ;  exit 0 ; } \
	&& tmux display-message \"Files sent to ssh #${ssh_ixs}\" ; sleep 2"
