#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: M. Tasić, C. Bürger

# Array of CJava source files ordered w.r.t. their compilation dependencies:
declare -a cjava_racr_sources=(
	exception-api	
	ast
	lexer
	parser
	unparser
	lexer-cl
	parser-cl
	support-api
	name-analysis
	compositions
	well-formedness
	main)

# Store current working directory:
old_pwd=`pwd`
	
if which javac
then
	echo "=========================================>>> Install CJava for Java:"
	cd cjava-jastadd
	sh build.bash
	cd $old_pwd
fi

cd $old_pwd

if which plt-r6rs
then
	echo "=========================================>>> Install CJava for Racket:"
	
	cd cjava-racr
	rm -rf racket-bin
	mkdir racket-bin
	for f in ${cjava_racr_sources[@]}
	do
		plt-r6rs ++path ./../../racr/racr/racket-bin --install --collections ./racket-bin $f.scm
	done
fi

cd $old_pwd

if which larceny
then
	echo "=========================================>>> Install CJava for Larceny:"
	
	cd cjava-racr
	
	# Delete old binaries:
	rm -rf larceny-bin
	mkdir larceny-bin
	mkdir larceny-bin/cjava-racr
	
	# Copy source files:
	for f in *.scm
	do
		cp -p $f larceny-bin/cjava-racr/${f%.*}.sls
	done
	
	# Create compile script
	cd larceny-bin
	cd cjava-racr	
	echo "#!r6rs" > compile-stale
	echo "(import (larceny compiler))" >> compile-stale
	echo "(compile-stale-libraries)" >> compile-stale
	
	# Execute compile script: #TODO: Fix relative path to RACR binaries when merged into RACR git repository!
	larceny --r6rs --path "./../../../../racr/racr/larceny-bin:./.." --program compile-stale
	
	# Delete compile script:
	rm compile-stale
fi

cd $old_pwd
