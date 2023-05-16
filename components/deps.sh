DEPS_APT="gcc make autoconf automake pkg-config zlib1g-dev curl python3-pip
		  cpanminus perl build-essential libncurses-dev libgnutls28-dev xclip
		  silversearcher-ag vlc pavucontrol flameshot ethtool bear flex bison htop arandr
		  fzf clang rofi"
# for Linux kernel compilation
DEPS_APT+=" libssl-dev libelf-dev"
DEPS_APT=$(echo $DEPS_APT | xargs)

function check_if_installed() {

	apt_output=$(eval apt-get install -s ${DEPS_APT})
	if [[ $? -gt 0 ]]; then
		echo "Failed to run apt to check installed dependencies"
		return 0
	fi

	new_packages_to_install=$(echo "${DEPS_APT}" | \
							  sed -En 's/^([0-9]+) upgraded, ([0-9]+) newly installed.*/\1 + \2/gp' | \
							  bc)
	if [[ ${new_packages_to_install} -eq 0 ]]; then
		return 0
	fi
	
	return 1
}

function install_component() {
	return 0
}
