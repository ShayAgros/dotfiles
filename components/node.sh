DEPS_APT=""

function check_if_installed() {
	if which node >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Installing node"

	mkdir -p ${SOFTWARE_DIR}/node >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/node

	wget --quiet https://nodejs.org/dist/latest/node-v20.1.0-linux-x64.tar.gz -O node.tar.gz
	tar --strip-components=1 -xf node.tar.gz

	sudo ln -s `pwd`/bin/node ${BIN_INSTALL_DIR}/node
	sudo ln -s `pwd`/bin/npm  ${BIN_INSTALL_DIR}/npm
	sudo ln -s `pwd`/bin/npx  ${BIN_INSTALL_DIR}/npx

	return 0
}
