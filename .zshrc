# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

export PATH=${HOME}/.local/bin:${PATH}

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"
# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git fzf sudo aws)

# Stop escaping urls (this is a nightmare to support this behaviour in scripts)
DISABLE_MAGIC_FUNCTIONS=true

source $ZSH/oh-my-zsh.sh

export EDITOR=nvim
alias vim=nvim

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

#export PATH="/Users/shayagr/.pyenv/bin:$PATH"
#eval "$(pyenv init -)"
#eval "$(pyenv virtualenv-init -)"

# jump over dots as well when traversing words
export WORDCHARS=.

function has_git_changed()
{
	remote_branch=$1
	if [[ -z ${remote_branch} ]]; then
		echo "Usage: has_git_changed [previous patchset branch]"
		return
	fi

	current_commit_hash=$(git show | git patch-id --stable | awk '{print $1}')
	current_commit_first_line=$(git log --pretty="%s" HEAD~1..HEAD)

	# search only the last 50 commits. Otherwise it would take forever to finish
	previous_version_commits=$(git log --reverse --pretty="%h" --committer=shayagr ${remote_branch}~50..${remote_branch})

	if [[ -z ${previous_version_commits} ]]; then
		echo "Failed to retrieve previous version's commits"
		return
	fi

	echo "${previous_version_commits}" | while read line; do
		test_commit_hash=$(git show ${line} | git patch-id --stable | awk '{print $1}')
		test_commit_first_line=$(git log --pretty="%s" ${line}~1..${line})

		if [[ ${current_commit_hash} == ${test_commit_hash} ]]; then
			same_commit=${line}
			break
		elif [[ ${current_commit_first_line} == ${test_commit_first_line} ]]; then
			immidiate_suspect_commit=${line}
		fi
	done

	if [[ ! -z ${same_commit} ]]; then
		echo "Current HEAD commit has the same patch hash as commit ${same_commit}"
	elif [[ ! -z ${immidiate_suspect_commit} ]]; then
		echo "Couldn't find a commit with the same hash"
		echo "But commit: " $(git log --pretty="%h %s" ${immidiate_suspect_commit}~1..${immidiate_suspect_commit})
		echo "has the same title"
	else
		echo "Couldn't find similar commit to current HEAD"
	fi

	# TODO: there must be a more elegant way of handling it. Currently we make
	# sure that subsequent calls don't use the previous value of these variables
	same_commit=""
	immidiate_suspect_commit=""
}

function vdiff() {
	# if the first argument wasn't provided, than diff against HEAD
	if [[ -z $1 ]]; then
		vim -p $(git --no-pager diff --ignore-submodules --name-only --relative HEAD) +"tabdo Gdiff" +"set foldlevel=99" +"tabdo 2wincmd w" +tabr
	else
		compared_commit=${1}
		diff_files=$(git --no-pager diff --ignore-submodules --name-only --relative ${compared_commit} ${compared_commit}~1)
		diff_compared_to_previous_HEAD=$(git --no-pager diff --ignore-submodules --name-only --relative HEAD HEAD~1)

		# exclude files that the patch doesn't modify compared to previous HEAD
		echo "${diff_files}" | while read line; do
			echo "${diff_compared_to_previous_HEAD}" | grep -q ${line} >/dev/null 2>&1 && vim_files="${vim_files} ${line}"
		done

		if [[ -z ${vim_files} ]]; then
			echo "No new changes are introduces in the version"
			return
		fi

		vim_files=$(echo ${vim_files} | xargs echo)
		echo "Opening files: " ${vim_files}
		eval vim -p "${vim_files}" +'"tabdo Gdiff ${compared_commit}" +"tabdo 2wincmd w"' +tabr
	fi
}

function send_script() {
	destination=$1

	script_files_names=$(ls -1 ~/workspace/scripts/*.sh | xargs -I{} basename {})

	files_to_send=$(echo "${script_files_names}" |
					rofi -dmenu -multi-select -i -p "Choose scripts to send" |
					xargs -I{} echo "~/workspace/scripts/{}" |
					xargs)

	eval ~/workspace/scripts/send_file.sh ${destination} ${files_to_send}
}

function startup_daemons() {
	tmux split-window -h
}

function sscreen() {

}

enable_kvm () {
	az=$1
	ip=$2

	defines=`gtsinit2_creds.sh $az`

	ssh g-$az "$defines; coap -Z $az -Y -m PUT coaps+tcp://$ip/api-v1/debug/utils/kvm/enable"
	ssh g-$az "$defines; coap -Z $az -Y -m PUT coaps+tcp://$ip/api-v1/debug/cosmicd/kvm/enable"
}

function exec_git() {
	GIT=$(which git)

	eval ${GIT} $@
}

function sda() {
	tmux rename-session daemons

	# kill all panes but the current one
	tmux list-panes -F '#{pane_active} #{session_id}:#{window_id}.#{pane_id}' | \
				awk '/^1/ {print "-t " $2}' | xargs tmux kill-pane -a

	tmux split-window -d -h -l 30% '~/.local/envs/awsh_env/bin/python3 ~/workspace/scripts/awsh_wip/awsh.py server'
	tmux split-window -d -v -l 50% '~/workspace/scripts/get_from_tmux_pane_daemon.sh'
	~/workspace/scripts/mbsync_all.sh
}

function co() {
	command_wo_co=${@}

	other_pane_dir=$(tmux list-panes -F '#{pane_active} #{pane_current_path}' | \
					 awk '/^0/ { print $2 }' | head -n 1)

	if [[ -z ${command_wo_co} ]]; then
		echo Please provide files to copy
		return
	fi

	if [[ -z ${other_pane_dir} ]]; then
		echo "Couldn't extract other pane's path"
	fi

	eval cp ${command_wo_co} ${other_pane_dir}
}

# make sure clang-format in path. needed for release script
export PATH=$PATH:/usr/share/clang/clang-format-10
# add toolbox to path (used to build FW)
export PATH=$PATH:~/.toolbox/bin:$PATH
# add gphackces to path
#export PATH=$PATH:~/src/ena-tools/ena/hackcess/

# THings needed for nvim
#eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)

# add ssh-agent keys
#source /Users/shayagr/workspace/scripts/agent_add.sh

# TODO: This needs to be converted into a daemon that refreshes the tickets once
# a day at most
function choose_sim_patch()
{
	export CAPATH=`ls -d /apollo/env/envImprovement/etc/cacerts /etc/ssl/certs 2>/dev/null | head -1`
	URL='https://maxis-service-prod-dub.amazon.com/issues?q=status%3AOpen+AND+assignee:shayagr'
	#URL='https://maxis-service-prod-dub.amazon.com/issues?q=containingFolder%3A9c88c706-7ea3-4454-bd2b-0e4d7844dc03&sort=lastUpdatedConversationDate'

	if [[ ! -f /tmp/sim_tickets ]]; then
		curl -sS  --anyauth --location-trusted -u: -c ~/.curl-cookies -b ~/.curl-cookies --capath "$CAPATH" "$URL" > /tmp/sim_tickets
		if [[ $? -gt 0 ]]; then
			echo "Failed to query SIM system. Did you run kinif -f ?"
			return
		fi
	fi

	choice=$(cat /tmp/sim_tickets | jq -r '.documents[] | [.aliases[].id, .title] | join("\t\t")' | fzf)
	[[ $? -eq 0 ]] || return

	echo "${choice}"
	# copy it to clipboard as well
	echo "${choice}" | xclip -selection clipboard
}

function get_ena_drivers_base_dir()
{
	echo "$(pwd | sed -E 's#ena-drivers(/?)#\n#g' | head -n -1 | tr -d '\n')/ena-drivers"
}

alias conf="~/workspace/scripts/instance_configure.sh"
alias con="~/workspace/scripts/connect.sh"
alias ix="~/workspace/scripts/exec_com_in_instance.sh"
alias send="~/workspace/scripts/send_file.sh"
alias syncc="~/workspace/scripts/send_changed_files.sh"
alias updateci="~/workspace/scripts/update_cli.sh"
alias get="~/workspace/scripts/get_from_device.sh"
alias sget="~/workspace/scripts/sftp_device.sh"
alias ac="~/workspace/scripts/add_connection.sh"
alias cs="sed -i 'd' ~/saved_instances/saved_logins"
alias gp='DIR=$(find ~/workspace/patches -maxdepth 2 -type d | fzf) && cd ${DIR}'
alias ga='DIR=$(find ~/workspace/amazon_repos -maxdepth 1 -type d | fzf) && cd ${DIR}'
alias v='VFILE=$(fzf) && print -s "vim ${VFILE}" && nvim ${VFILE}' # print -s adds the vim ${file} command to history
alias grep='grep --color'
alias chp="~/linux/scripts/checkpatch.pl"
# get kibana log
alias gkl="~/workspace/scripts/kibana_to_efa_tools.sh"
alias vshow='vim -p $(git --no-pager diff --ignore-submodules --name-only --relative HEAD HEAD~1) +"tabdo leftabove Gvdiffsplit HEAD~1" +"tabdo 2wincmd w" +"0Gtabedit @" +"set foldlevel=0" +"tabdo diffupdate" +tabr'
#alias vdiff='vim -p $(git --no-pager diff --ignore-submodules --name-only --relative HEAD) +"tabdo Gdiff" +tabr'
alias utags=~/workspace/Software/ctags/ctags
# get 20 largest files
alias dul='du -ah --block-size=M | sort -n -r | head -n20'
alias crr='~/src/Git-review-tools/bin/git-post-commits origin master'
alias create_dump_commit='~/workspace/scripts/create_dump_commit.sh'
alias fs=~/workspace/scripts/find_similar.sh # search for a similar commit in another branch
# Remove ena_release directory in whatever ena-drivers repo I'm in
alias dr="rm -rf \$(get_ena_drivers_base_dir)/tools/{upstream_release,external_git_release}/ena_release"
alias ssc=send_script
alias agui="~/.local/envs/awsh_env/bin/python3 ~/workspace/scripts/awsh/awsh_gui.py"
alias aserver="~/.local/envs/awsh_env/bin/python3 ~/workspace/scripts/awsh/awsh.py server"
alias awsh="~/.local/envs/awsh_env/bin/python3 ~/workspace/scripts/awsh/awsh.py"
alias sd=startup_daemons
alias xc="xclip -selection clipboard"
alias et="emacsclient -nw"
alias e="emacsclient --create-frame"
alias efa_tool="~/.local/envs/efa_tool_env/bin/python3 ~/src/ena-tools/efa/efa_tool/efa_tool.py"
alias vcon="~/workspace/scripts/connect_vnc.sh"
alias sp="nvim +'set buftype=nofile' -"
alias today='date -d "now" +"%d_%m_%Y"'
alias pc='~/workspace/scripts/create_project.sh'
alias csp=choose_sim_patch
alias greview='git rebase -i $(git merge-base origin/master HEAD)'
alias ls="ls -t --color"
#alias git='exec_git'

#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH=$HOME/.toolbox/bin:$PATH

export WORKON_HOME=~/.local/envs
export VIRTUALENVWRAPPER_PYTHON=`which python3`
source /usr/local/bin/virtualenvwrapper.sh

export PATH=$HOME/.toolbox/bin:$PATH
export PATH=~/workspace/Software/node/node-v18.15.0-linux-x64/bin:$PATH
export PATH=/home/ANT.AMAZON.COM/shayagr/go/bin:$PATH

# add clangd to path
export PATH=/home/ANT.AMAZON.COM/shayagr/workspace/Software/clangd/clangd_14.0.0/bin:$PATH

alias bb=brazil-build

# gerrit website directory
export GERRIT_SITE=~/workspace/gerrit_testsite

# Configure Lua
#export PATH=${PATH}:/opt/carbon/bin/
#export LUA_PATH='/usr/share/lua/5.3/?/init.lua;/usr/share/lua/5.3/?.lua;/home/ANT.AMAZON.COM/shayagr/workspace/Software/luarocks/luarocks-3.8.0/./lua_modules/share/lua/5.3/?.lua;/home/ANT.AMAZON.COM/shayagr/workspace/Software/luarocks/luarocks-3.8.0/./lua_modules/share/lua/5.3/?/init.lua;/home/ANT.AMAZON.COM/shayagr/.local/share/lua/5.3/?.lua;/home/ANT.AMAZON.COM/shayagr/.luarocks/share/lua/5.3/?.lua;/home/ANT.AMAZON.COM/shayagr/.luarocks/share/lua/5.3/?/init.lua;/usr/local/share/lua/5.3/?.lua;/usr/local/share/lua/5.3/?/init.lua;/usr/local/lib/lua/5.3/?.lua;/usr/local/lib/lua/5.3/?/init.lua;/usr/share/lua/5.3/?.lua;/usr/share/lua/5.3/?/init.lua;./?.lua;./?/init.lua;/opt/carbon/share/lua/5.3/?.lua;/home/ANT.AMAZON.COM/shayagr/.local/share/lua/5.3/?/init.lua;'${LUA_PATH}
#export LUA_CPATH='/usr/lib/x86_64-linux-gnu/lib?.so;/home/ANT.AMAZON.COM/shayagr/workspace/Software/luarocks/luarocks-3.8.0/./lua_modules/lib/lua/5.3/?.so;/home/ANT.AMAZON.COM/shayagr/.luarocks/share/lua/5.3/?.lua;/home/ANT.AMAZON.COM/shayagr/.luarocks/share/lua/5.3/?/init.lua;/usr/local/share/lua/5.3/?.lua;/usr/local/share/lua/5.3/?/init.lua;/usr/local/lib/lua/5.3/?.lua;/usr/local/lib/lua/5.3/?/init.lua;/usr/share/lua/5.3/?.lua;/usr/share/lua/5.3/?/init.lua;./?.lua;./?/init.lua;/opt/carbon/share/lua/5.3/?.lua;/opt/carbon/lib/lua/5.3/?.so;/home/ANT.AMAZON.COM/shayagr/.luarocks/lib/lua/5.3/?.so;/home/ANT.AMAZON.COM/shayagr/.local/lib/lua/5.3/?.so;'${LUA_CPATH}
#export PATH='/home/ANT.AMAZON.COM/shayagr/workspace/Software/luarocks/luarocks-3.8.0/./lua_modules/bin:/home/ANT.AMAZON.COM/shayagr/.luarocks/bin:'${PATH}
#export LUA_PATH="${LUA_PATH};/opt/carbon/share/lua/5.3/?.lua"
#export LUA_CPATH="${LUA_CPATH};/opt/carbon/lib/lua/5.3/?.so"

#export LUA=/usr/include/lua5.3
#export LUAINC=/usr/include/lua5.3
#export LUALIB=/usr/lib/x86_64-linux-gnu
#export LUABIN=/usr/bin

export PATH="/usr/local/opt/node@14/bin:$PATH"
# gp-hackcess setup
source /home/ANT.AMAZON.COM/shayagr/.gph-setup

export KERNEL_BUILD_DIR=~/linux

PATH="/home/ANT.AMAZON.COM/shayagr/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/ANT.AMAZON.COM/shayagr/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/ANT.AMAZON.COM/shayagr/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/ANT.AMAZON.COM/shayagr/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/ANT.AMAZON.COM/shayagr/perl5"; export PERL_MM_OPT;

alias luamake=/home/ANT.AMAZON.COM/shayagr/workspace/Software/lua-language-server/3rd/luamake/luamake

# add lua language server to path
export PATH="/home/ANT.AMAZON.COM/shayagr/workspace/Software/lua-language-server/bin:${PATH}"
