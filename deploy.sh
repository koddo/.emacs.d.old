#!/usr/bin/env bash

cd "$(dirname "$0")"

cat > ym-packages/org-mode/local.mk << EOF
EMACS = /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
EOF
cd ym-packages/org-mode && make

echo
echo
echo
echo 'now do $ ln -s preinit_smth.el preinit_this_machine.el'

