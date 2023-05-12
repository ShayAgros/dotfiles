#!/usr/bin/env bash

tmux display-popup -E "\
        tmux list-sessions -F '#{?session_attached,,#{session_last_attached} #{session_name}}' |\
        sed '/^$/d' |\
        sort -rn |\
        cut -d' ' -f2- |\
        fzf --reverse --header jump-to-session --preview 'tmux capture-pane -pt {}' |\
        xargs -I {} tmux move-window -t \"{}\""
