#!/usr/bin/env bash

force="n"

for file in `ls`; do

	[[ $file == `basename $0` ]] && continue

	if `ls ~/.$file >/dev/null 2>&1`  && [[ ${force} == "n" ]]; then
		read -p "file ~/.${file} exists. Do you wish to replace it (y/n)? " -n1 ans
		echo ""
		[[ ${ans} == "y" ]] || continue
	fi

	rm ~/.${file}
	ln -s `pwd`/${file} ~/.${file}
done
