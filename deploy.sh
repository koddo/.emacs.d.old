#!/usr/bin/env bash

cd "$(dirname "$0")"

# TODO: this must depend on machine
rm preinit_this_machine.el
ln -s preinit_mbp985.el preinit_this_machine.el


cat > ym-packages/org-mode/local.mk << EOF
EMACS = /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
EOF
cd ym-packages/org-mode && make



