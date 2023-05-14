DEPS_APT=""

function check_if_installed() {
	if which lua-language-server >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {

	mkdir -p ${SOFTWARE_DIR}/lua-language-server >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/lua-language-server

	wget --quiet \
		https://github.com/LuaLS/lua-language-server/releases/download/3.6.19/lua-language-server-3.6.19-linux-x64.tar.gz \
		-O lua-language-server.tar.gz

	tar -xf lua-language-server.tar.gz
	ln -s `pwd`/bin/lua-language-server ${BIN_INSTALL_DIR}/lua-language-server

	return 0
}
