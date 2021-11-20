#!/usr/bin/env bash

themes="
blinks
afowler
cypher
daveverwer
dst
dpoggi
dstufft
"

which rofi >/dev/null 2>&1 || { tmux display-message "Please install rofi to change them" ; exit 1 ; }

theme=$(echo -e "${themes}" | sed '/^$/d' | rofi -dmenu -i -p "Choose choose theme to set")

# allow the user to just exit
[[ $? == 0 ]] || exit 0

tmux send-keys "C-u"
# TODO: make it a .zsh plugin. It is very ugly this way
#tmux send-keys -l "sed -ie \"s/ZSH_THEME=.*/ZSH_THEME=\\\"${theme}\\\"/\" ~/.zshrc"
#tmux send-keys "Enter"
tmux send-keys -l "omz theme set ${theme}"
tmux send-keys "Enter"

tmux display-message "Changed theme to ${theme}"
