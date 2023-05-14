DEPS_APT="zsh"

function check_if_installed() {

	if [[ -d ~/.oh-my-zsh ]]; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Configuring oh-my-zsh"

	yes | sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
	ln -sf ${DOTS_DIR}/.zshrc ~/.zshrc

	echo Making zsh the default shell
	if which amazon-config-write >/dev/null ; then
		sudo amazon-config-write override_shell `which zsh`
		sudo manage --force
	fi

	return 0
}
