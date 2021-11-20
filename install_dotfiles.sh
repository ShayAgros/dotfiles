#!/usr/bin/env bash

force="n"
set -e

function install_programs {
	if which pacman > /dev/null 2>&1 ; then
		sudo pacman -Syuu ${arch_deps}
	elif which apt-get > /dev/null 2>&1 ; then
		sudo apt-get install ${ubuntu_deps} -y
	fi
}

function install_tmux {

	echo configuring tmux

	tmux_dir=~/.config/tmux
	if [[ -d ${tmux_dir} ]]; then
		echo "WARNING: tmux diretory already exists (${tmux_dir}). Not overriding it"
		return
	fi

	ln -s `pwd`/tmux ${tmux_dir}

	# install tmux plugin manager
	mkdir -p ${tmux_dir}/plugins

	git clone https://github.com/tmux-plugins/tpm ${tmux_dir}/plugins/tpm
}

# terminal emulator
function install_kitty {

	if ! which kitty >/dev/null 2>&1; then
		echo "Installing kitty"
		mkdir -p ~/workspace/Software/kitty >/dev/null 2>&1 || true
		cd ~/workspace/Software/kitty
		wget https://github.com/kovidgoyal/kitty/releases/download/v0.19.3/kitty-0.19.3-x86_64.txz >/dev/null
		tar xf kitty-*
		sudo ln -s `pwd`/bin/kitty /usr/bin/kitty
		cd -
	fi

	echo configuring kitty

	mkdir -p ~/.config/kitty || true
	ln -fs `pwd`/kitty.conf ~/.config/kitty/
}

# install FiraCode Nerd font
function install_font {
	wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip >/dev/null

	mkdir -p ~/.local/share/fonts/FiraCode 2>/dev/null || true
	mv FiraCode.zip ~/.local/share/fonts/FiraCode/

	cd ~/.local/share/fonts/FiraCode
	unzip FiraCode.zip
	cd -

	fc-cache -v ~/.local/share/fonts
}

# install oh-my-zsh with custom .zshrc
function install_zsh {
	yes | sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

#	ln -sf `pwd`/.zshrc ~/.zshrc
	# though not directly related to zsh, it makes more sense to put it after we
	# know that zsh is configured
	echo 'export PATH=$PATH:~/.luarocks/bin' >> ~/.zshrc
}

function install_zathura {
	mkdir -p ~/.config/zathura

	cp `pwd`/zathurarc ~/.config/zathura
}

function configure_python {

	echo installing python packages

	# not sure if it is even needed

	distro_name=$(cat /etc/os-release | awk -F'"' '/^NAME=/ {print $2}')
	# extract 20 from the output VERSION_ID="20.04"
	ubuntu_version=$(cat /etc/os-release | awk -F'"|\.' '/VERSION_ID/ {print $2}')
	if [[ ${distro_name} == Ubuntu ]] && (( ubuntu_version < 20 )) ; then
		# pip2
		pip install neovim
		pip install git-review
		pip install lxml requests
		pip install libclang
		pip install ply
		pip install gitpython
	fi

	#pip3
	pip3 install neovim
	pip3 install git-review
	pip3 install cmake # Ubuntu package is too old
	pip3 install lxml requests
	pip3 install libclang
	pip3 install ply
	pip3 install gitpython
}

function configure_perl
{
	echo "configuring perl"
	cpanm Neovim::Ext
}

function configure_keyboard
{
	echo configuring keyboard layout and options
	sudo mkdir /etc/X11/xorg.conf.d
	sudo cp -i ./00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf
}

function configure_node
{
	if ! which node >/dev/null 2>&1 ; then
		echo "installing node"
		mkdir -p ~/workspace/Software/node >/dev/null 2>&1 || true
		cd ~/workspace/Software/node
		wget https://nodejs.org/dist/v14.16.0/node-v14.16.0-linux-x64.tar.xz >/dev/null
		tar xf node*
		cd node-v14.16.0-linux-x64
		
		sudo ln -s `pwd`/bin/node /usr/bin/node
		sudo ln -s `pwd`/bin/npm /usr/bin/npm
		sudo ln -s `pwd`/bin/npx /usr/bin/npx
		cd -
	fi


	echo "configuring node"
	npm install -g neovim
}

function configure_lua
{
	luarocks --local install lgi
	luarocks --local install ldoc
}

function configure_nvim
{
	if ! which nvim >/dev/null 2>&1; then
		echo "Installing neovim"
		mkdir -p ~/workspace/Software/nvim >/dev/null 2>&1 || true
		cd ~/workspace/Software/nvim
		wget https://github.com/neovim/neovim/releases/download/v0.4.4/nvim.appimage
		chmod +x nvim.appimage
		sudo ln -s `pwd`/nvim.appimage /usr/bin/nvim
		cd -
	fi
}

function configure_ruby
{
	sudo gem install neovim
}

function configure_picom
{
	if ! which picom >/dev/null 2>&1; then
		echo installing picom

		mkdir -p ~/workspace/Software/picom >/dev/null 2>&1 || true
		pushd ~/workspace/Software/picom
		wget https://github.com/yshui/picom/archive/v8.2.tar.gz
		tar xf v8.2.tar.gz
		pushd picom-8.2

		meson --buildtype=release . build
		ninja -C build
		sudo ln -s `pwd`/build/src/picom /usr/bin/picom
		popd
		popd
	fi

	mkdir ~/.config/picom 2>/dev/null || true
	ln -sf `pwd`/picom.conf ~/.config/picom/picom.conf
}

function configure_emacs
{
	if ! which emacs >/dev/null 2>&1; then
		echo installing emacs
		mkdir -p ~/workspace/Software/emacs >/dev/null 2>&1 || true
		pushd ~/workspace/Software/emacs
		wget http://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz
		tar xf emacs-27.1.tar.xz
		pushd emacs-27.1
		./configure
		make -j $(getconf _NPROCESSORS_ONLN)
		sudo ln -sf `pwd`/src/emacs /usr/bin/emacs
		sudo ln -sf `pwd`/lib-src/emacsclient /usr/bin/emacsclient
		popd
		popd
	fi
}

function configure_mu
{
	if ! which mu >/dev/null 2>&1; then
		echo installing mu
		mkdir -p ~/workspace/Software/mu >/dev/null 2>&1 || true
		pushd ~/workspace/Software/mu
		wget https://github.com/djcb/mu/releases/download/1.4.15/mu-1.4.15.tar.xz
		tar xf mu-1.4.15.tar.xz
		pushd mu-1.4.15
		./configure
		make -j $(getconf _NPROCESSORS_ONLN)
		sudo ln -sf `pwd`/mu/mu /usr/bin/mu
		popd
		popd
	fi
}

function configure_ctags
{
	if ! which ctags >/dev/null 2>&1; then
		echo installing ctags
		mkdir -p ~/workspace/Software/ctags >/dev/null 2>&1 || true
		pushd ~/workspace/Software/ctags
		wget https://github.com/universal-ctags/ctags/archive/p5.9.20210307.0.tar.gz
		tar xf p5.9.20210307.0.tar.gz
		pushd ctags-p5.9.20210307.0
		./autogen.sh
		./configure --prefix=/usr
		make -j $(getconf _NPROCESSORS_ONLN)
		sudo ln -sf `pwd`/ctags /usr/bin/ctags
		popd
		popd
	fi
}

ubuntu_deps="gcc make autoconf automake pkg-config zlib1g-dev curl
	python3-pip cpanminus perl libxapian-dev libgmime-3.0-dev isync cscope
	build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev
	libtiff-dev libgtk-3-dev libncurses-dev libgnutls28-dev ruby ruby-dev xclip
	silversearcher-ag liblua5.3-dev vlc virtualbox-6.1 pavucontrol flameshot
	ethtool bear flex bison tmux zathura zsh htop cmake arandr fzf clang rofi"

# For awesomeWM
ubuntu_deps="${ubuntu_deps} lua5.3 luarocks libgirepository1.0-dev asciidoctor libxcb-cursor-dev libxcb-randr0-dev
			 libxcb-xtest0-dev libxcb-xinerama0-dev libxcb-shape0-dev libxcb-util-dev libxcb-keysyms1-dev libxcb-icccm4-dev
			 libxcb-xfixes0-dev libxcb-xkb-dev libxkbcommon-x11-dev libstartup-notification0-dev libxdg-basedir-dev libxcb-xrm-dev"

# For picom
ubuntu_deps="${ubuntu_deps} libxext-dev libxcb1-dev libxcb-damage0-dev
			 libxcb-render-util0-dev libxcb-render0-dev libxcb-composite0-dev
			 libxcb-image0-dev libxcb-present-dev libxcb-glx0-dev libpixman-1-dev
			 libdbus-1-dev libconfig-dev libgl1-mesa-dev libpcre2-dev libpcre3-dev
			 libevdev-dev uthash-dev libev-dev libx11-xcb-dev meson ninja-build"

# for email (mbsync + mu4e)
ubuntu_deps="${ubuntu_deps} isync w3m libxml2-utils"

arch_deps="kitty tmux emacs neovim wget zip zsh curl zathura fzf"

#install_programs

#install_zsh
#install_font
#install_kitty
#install_tmux
#install_zathura
#configure_keyboard
#configure_python
#configure_ruby
#configure_perl
#configure_node
#configure_lua
#configure_nvim
#configure_picom
#configure_emacs
#configure_mu
configure_ctags

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
