DEPS_APT="lua5.3 liblua5.3-dev luarocks libgirepository1.0-dev asciidoctor libxcb-cursor-dev libxcb-randr0-dev
		  libxcb-xtest0-dev libxcb-xinerama0-dev libxcb-shape0-dev libxcb-util-dev libxcb-keysyms1-dev libxcb-icccm4-dev
		  libxcb-xfixes0-dev libxcb-xkb-dev libxkbcommon-x11-dev libstartup-notification0-dev libxdg-basedir-dev libxcb-xrm-dev
		  libcairo2-dev libgdk-pixbuf-2.0-dev"

function check_if_installed() {
	if which awesome >/dev/null ; then
		return 0
	fi

	return 1
}

function install_component() {

	echo "Installing luarocks dependencies"

	sudo luarocks install lgi
	luarocks --local install ldoc

	echo "Installing AwesomeWM"
	
	cd ${SOFTWARE_DIR}
	git clone --quiet git@github.com:awesomeWM/awesome.git awesome
	cd awesome
	make >/dev/null
	sudo make install >/dev/null

	return 0
}
