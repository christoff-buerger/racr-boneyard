#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: A. Misiuda

if [ ! $# == 1 ]
then
	echo "Wrong number of arguments - two arguments expected:"
	echo "	(1) Scheme distribution directory"
	exit 1
fi
if [ ! -d $1 ]
then
	echo "The given Scheme distribution directory [$1] does not exist."
	exit 2
fi

declare -a libs=(
	rgd/types
	util
	spec/ast
	spec/print
	spec/node-status
	spec/node-data
	spec/data
	spec/mathing-cmds
	rgd/model
	rgd/rag-doll
)

declare -a racket=(
	/lib/gui/		windows
	/lib/gui/		gui-menu
	/lib/gui/lists/		list-class
	/lib/gui/lists/		names-list
	/lib/gui/lists/		types-list
	/lib/gui/lists/		references-list
	/lib/gui/		rag-doll-connector
	/lib/gui/		gui
	/			main
)


declare -a petrinets_sources=(
	test-rules
	test-rules-refs
)

declare n path fname

declare collects=$1/collects

if [ ! -d $collects ]
then
	collects=$1/lib/racket/collects
fi

if [ -f $1/bin/plt-r6rs ]
then
	echo "Installing Rag-Doll's RACR components..."
	# Delete old:
	rm -r $collects/rag-doll

	# Install new:
	for (( i=0; i<${#libs[*]}; i++ )) do
		$1/bin/plt-r6rs --all-users --install ./lib/${libs[i]}.scm
	done

	echo "Installing Rag-Doll's Racket components..."
	n=${#racket[*]}
	mkdir ./racket-compile
	if [ ! -d ./racket-compile ]
	then
		mkdir $collects/rag-doll/compiled
	fi
	mkdir $collects/rag-doll/lib
	for ((i=0; i<n; ))
	do
		path=${racket[i++]}
		fname=${racket[i++]}
		echo "compiling $path$fname..."
		cp .$path$fname.rkt ./racket-compile/$fname
		path=$collects/rag-doll$path
		$1/bin/raco make ./racket-compile/$fname
		if [ ! -d $path ]
		then
			mkdir $path
			mkdir $path/compiled
		fi
		cp ./racket-compile/$fname $path$fname.ss
		mv ./racket-compile/compiled/$fname.zo $path"compiled"
	done
	rm -R ./racket-compile

	# copy sources
	cp -r ./src $collects/rag-doll/
fi
