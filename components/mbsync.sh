DEPS_APT="libsasl2-dev"

function check_if_installed() {
	if which mbsync >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {
	echo "Installing isync (mbsync)"

	cd ${SOFTWARE_DIR}
	git clone --quiet git://git.code.sf.net/p/isync/isync isync
	cd isync

	./autogen.sh
	./configure
	make

	sudo ln -s `pwd`/src/mbsync ${BIN_INSTALL_DIR}/mbsync

	mkdir ~/workspace/mail
	mkdir ~/workspace/mail/gmail
	mkdir ~/workspace/mail/amazon

	ln -s ${DOTS_DIR}/.mbsyncrc ~/.mbsyncrc

	return 0
}
