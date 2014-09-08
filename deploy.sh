#!/usr/bin/env bash

cd "$(dirname "$0")"

cd ym-packages/org-mode && make

echo
echo
echo
echo 'now do $ ln -s preinit_smth.el preinit_this_machine.el'

