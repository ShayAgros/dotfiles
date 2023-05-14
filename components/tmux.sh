DEPS_APT="libevent-dev"

function check_if_installed() {
	if which tmux >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Installing tmux"

	mkdir -p ${SOFTWARE_DIR}/tmux >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/tmux

	wget --quiet https://github.com/tmux/tmux/releases/download/3.3/tmux-3.3.tar.gz -O tmux.tar.gz
	tar --strip-components=1 -xf tmux.tar.gz

	./configure > /dev/null
	make -j `nproc` >/dev/null

	sudo ln -s `pwd`/tmux ${BIN_INSTALL_DIR}/tmux

	tmux_dir=~/.config/tmux
	ln -s ${DOTS_DIR}/tmux ${tmux_dir}
	mkdir -p ${tmux_dir}/plugins
	git clone --quiet https://github.com/tmux-plugins/tpm ${tmux_dir}/plugins/tpm

	return 0
}
