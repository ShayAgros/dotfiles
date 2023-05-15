DEPS_APT="meson libxapian-dev libgmime-3.0-dev guile-3.0-dev"

function check_if_installed() {
	if which mu >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {

	echo "Installing mu and mu4e"

	if ! which emacs >/dev/null ; then
		echo "Error! emacs is not installed"
		return 1
	fi

	wget --quiet https://github.com/djcb/mu/releases/download/v1.10.3/mu-1.10.3.tar.xz -O mu.tar.xz
	tar --strip-components=1 -xf mu.tar.xz

	./autogen.sh
	make

	sudo ln -s `pwd`/build/mu/mu ${SOFTWARE_DIR}/mu

	return 0
}
