#!/usr/bin/env bash

PKGCMD=$1
PKGNAME=$2
PKGGIT=$3
PKGREF=$4

cd "$(dirname "$0")"


function usage() {
    echo "usage: ym-packages.sh <add|pull|remove> name git ref"
    echo
    echo "commit your changes before using"
    echo
    echo "note: You can get specific commit if needed by adding or pulling by it's ref"
    echo 
    echo "git subtree doesn't support remembering repositories"
    echo "and everything out of the box so I just use ym-packages.log"
}
[[ -n $(git status -s) ]] && \
    echo "$(usage)" && exit 1


function log_entry() {    # extract from 'Squashed ... content from commit 75e85d0'
    echo "$PKGNAME $PKGGIT $PKGREF -- $(echo `git log --grep Squashed --oneline | grep $PKGNAME | head -n 1 | awk '{print $NF}'`)"
}
if [[ $PKGCMD == "add" ]]; then
    git remote add $PKGNAME $PKGGIT && \
	git fetch $PKGNAME && \
	git subtree add -P ym-packages/$PKGNAME $PKGNAME/$PKGREF --squash && \
	echo "added $(log_entry)" >> ym-packages.log && \
	git add ym-packages.log && git commit -m "ym-packages.log: added $PKGNAME" && tail -n 1 ym-packages.log 
elif [[ $PKGCMD == "pull" ]]; then
    git subtree pull -P ym-packages/$PKGNAME $PKGNAME $PKGREF --squash && \
	echo "pulled $(log_entry)" >> ym-packages.log && \
	git add ym-packages.log && git commit -m "ym-packages.log: pulled $PKGNAME" && tail -n 1 ym-packages.log
elif [[ $PKGCMD == "remove" ]]; then
    git remote remove $PKGNAME && \
	git rm -r ym-packages/$PKGNAME && \
	echo "removed $(log_entry)" >> ym-packages.log && \
	git add ym-packages.log && git commit -m "ym-packages.log: removed $PKGNAME" && tail -n 1 ym-packages.log
else
    echo "$(usage)"
fi



