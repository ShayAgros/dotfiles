DEPS_APT="libfuse-dev"

function check_if_installed() {
	if which nvim >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {

	echo "Downloading and installing neovim"

	mkdir -p ${SOFTWARE_DIR}/nvim >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/nvim

	wget --quiet https://github.com/neovim/neovim/releases/download/stable/nvim.appimage
	chmod +x nvim.appimage

	sudo ln -s `pwd`/nvim.appimage /usr/local/bin/nvim

	# Configure VIMRC
	echo "Downloading vimrc"
	git clone git@github.com:ShayAgros/myVimrc.git ~/workspace/dotfiles/myVimrc 
	cd ~/workspace/dotfiles/myVimrc
	
	./replace_vimrc.sh

	return 0
}
