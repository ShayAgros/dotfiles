#!/usr/bin/env bash

force="n"

function install_programs {
	if which pacman > /dev/null 2>&1 ; then
		sudo pacman -Syuu ${@}
	fi
}

function install_tmux {

	echo configuring tmux
	ln -s `pwd`/tmux.conf ~/.tmux.conf

	# install tmux plugin manager
	mkdir -p ~/.tmux/plugins

	if [[ ! -d ~/.tmux/plugins/tpm ]]; then
		git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
	fi
}

# terminal emulator
function install_kitty {

	echo configuring kitty

	mkdir -p ~/.config/kitty
	ln -s `pwd`/kitty.conf ~/.config/kitty/
}

# install FiraCode Nerd font
function install_font {
	wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip

	mkdir -p ~/.local/share/fonts/FiraCode
	mv FiraCode.zip ~/.local/share/fonts/FiraCode/

	cd ~/.local/share/fonts/FiraCode
	unzip FiraCode.zip
	cd -

	fc-cache -v ~/.local/share/fonts
}

# install oh-my-zsh with custom .zshrc
function install_zsh {
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

#	ln -sf `pwd`/.zshrc ~/.zshrc
}

function install_zathura {
	mkdir -p ~/.config/zathura

	cp `pwd`/zathurarc ~/.config/zathura
}

function configure_python {

	echo installing python packages
	# pip2
	pip install neovim
	pip install git-review
	pip install lxml requests
	pip install libclang
	pip install ply
	pip install gitpython

	#pip3
	pip3 install neovim
	pip3 install git-review
	pip3 install cmake # Ubuntu package is too old
	pip3 install lxml requests
	pip3 install libclang
	pip3 install ply
	pip3 install gitpython
}

function configure_perf {
	cpanm Neovim::Ext
}

function configure_keyboard {
	echo configuring keyboard layout and options
	sudo cp -i ./00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf
}

ubuntu_deps=gcc make autoconf automake pkg-config zlib1g-dev node curl python-pip \
	python3-pip cpanminus perl libxapian-dev libgmime-3.0-dev isync cscope \
	build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev \
	libtiff-dev libgtk-3-dev libncurses-dev libgnutls28-dev ruby ruby-dev xclip \
	silversearcher-ag liblua5.3-dev vlc virtualbox-6.1 pavucotrol flameshot \
	ethtool

#install_programs kitty tmux emacs neovim wget zip zsh curl zathura fzf

#install_zsh
install_font
#install_kitty
#install_tmux
#install_zathura
#for file in `ls`; do

	#[[ $file == `basename $0` ]] && continue

	#if `ls ~/.$file >/dev/null 2>&1`  && [[ ${force} == "n" ]]; then
		#read -p "file ~/.${file} exists. Do you wish to replace it (y/n)? " -n1 ans
		#echo ""
		#[[ ${ans} == "y" ]] || continue
	#fi

	#rm ~/.${file}
	#ln -s `pwd`/${file} ~/.${file}
#done
