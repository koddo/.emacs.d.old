.emacs.d
========

# keybindings
in bettertouchtool for osx

| keybinding | exec command |
| --- | --- |
| C-S-F11 | osascript ~/.emacs.d/org-clock.applescript clock_in |
| C-S-M-F11 | osascript ~/.emacs.d/org-clock.applescript clock_in_default |
| C-S-F12 | osascript ~/.emacs.d/org-clock.applescript clock_out |
| C-S-F10 | osascript ~/.emacs.d/org-clock.applescript show_current |

TODO: same thing in linux


# ym-packages.sh
TODO: use elpa when it supports ssl or signed packages, it now doesn't

Anyway it's convenient to have everything in the same repo for fast deployment.

I use stock git subtrees without support of .gittrees file, in some forks it contains info on paths, repos, and refspecs.

Installation looked like:
```
commit uncommitted
$                 # assuming we are in ~/.emacs.d/
$ git remote add org-mode git://orgmode.org/org-mode.git
$ git fetch org-mode
$ git subtree add -P emacs.d/ym-packages/org-mode org-mode maint --squash
```

I wrote a trivial script which also logs info on subtrees:
```
./ym-packages.sh add org-mode git://orgmode.org/org-mode.git maint
```

# to-do

TODO: in org-clock.applescript find a better way to get full path of emacsclient --- in osx it's in Emacs.app, not one installed systemwide

