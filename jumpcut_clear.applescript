#!/usr/bin/osascript

## export this as an app (maybe even sign it) to ~/Applications and allow it in preferences->security->accesibility
## add to bettertouchtools as `osascript ~/Applications/jumpcut_clear.app`, not as `open -a`, to avoid `run or quit` dialog

launch application "System Events"
delay 0.2

ignoring application responses
	tell application "System Events" to tell process "Flycut"
		click menu bar item 1 of menu bar 1
	end tell
end ignoring

# http://stackoverflow.com/questions/16492839/applescript-on-clicking-menu-bar-item-via-gui-script/16497564#16497564
do shell script "killall System\\ Events"
delay 0.5

tell application "System Events" to tell process "Flycut"
	tell menu bar item 1 of menu bar 1
		click menu item "Clear All" of menu 0
	end tell
	delay 0.8
	click button "Clear" of window 1
end tell
