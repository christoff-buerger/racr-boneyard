#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: M. Tasić

# Ignore hidden directories and files
find . -path '*/.*' -prune -o -path '*/*~' -prune -o -print |
# Ignore generated directories and files:
grep -v TLang_java/javac_bin |
grep -v TLang_java/.*.jar |
grep -v measurements/..* |
# Zip all together
zip tlang.zip -@

