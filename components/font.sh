DEPS_APT=""

function check_if_installed() {
	if [[ -d ~/.local/share/fonts/firacode ]]
		return 0
	fi

	return 1
}

function install_component() {
	echo "Downloading font"

	mkdir -p ~/.local/share/fonts/firacode 2>/dev/null || true
	cd ~/.local/share/fonts/firacode

	wget --quiet https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip
	unzip FiraCode.zip

	fc-cache -v ~/.local/share/fonts

	return 0
}
