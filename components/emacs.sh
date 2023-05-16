DEPS_APT="libgtk-3-dev libwebkit2gtk-4.0-dev libjansson4 libjansson-dev gcc-11 libgccjit0 libgccjit-11-dev
		  libmagickwand-dev libgtk-3-dev libxpm-dev libgif-dev"
DEPS_APT=$(echo ${DEPS_APT} | xargs)

function check_if_installed() {

	if which emacs >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {

	echo "Installing emacs"

	mkdir -p ${SOFTWARE_DIR}/emacs >/dev/null 2>&1 || true
	cd ${SOFTWARE_DIR}/emacs

	wget --quiet http://mirror.rabisu.com/gnu/emacs/emacs-28.2.tar.xz -O emacs.tar.gz
	tar --strip-components=1 -xf emacs.tar.gz
	./autogen

	./configure --without-compress-install --with-native-compilation --with-json \
				--with-mailutils --with-native-compilation --with-imagemagick	 \
				--with-cairo --with-cairo --with-xwidgets --with-x-toolkit=gtk3
	make -j`nproc`

	ln -s ${DOTS_DIR}/emacs.d ~/.emacs.d

	return 0
}
