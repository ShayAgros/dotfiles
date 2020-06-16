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
	mkdir -s ~/.tmux/plugins

	if [[ ! -d ~/.tmux/plugins/tpm ]]; then
		git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
	fi
}

# terminal emulator
function install_kitty {

	echo configuring kitty

	mkdir ~/.config/kitty
	ln -s `pwd`/kitty.conf ~/.config/kitty/
}

# install FiraCode Nerd font
function install_font {
	wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip

	mkdir -p ~/.local/share/fonts/
	mv FiraCode.zip ~/.local/share/fonts/FiraCode

	cd ~/.local/share/fonts/FiraCode
	unzip FiraCode.zip
	cd -

	fc-cache -v ~/.local/share/fonts
}

# install oh-my-zsh with custom .zshrc
install zsh {
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

	ln -sf `pwd`/.zshrc ~/.zshrc
}

install_zathura {
	mkdir -p ~/.config/zathura

	cp `pwd`/zathurarc ~/.config/zathura
}

install_programs kitty tmux emacs neovim wget zip zsh curl zathura fzf

install_font
install_kitty
install_tmux
install_zathura
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
