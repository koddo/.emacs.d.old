#!/usr/bin/osascript

ignoring application responses
	tell application "System Events" to tell process "Flycut"
		click menu bar item 1 of menu bar 1
	end tell
end ignoring
do shell script "killall System\\ Events"
delay 0.1
tell application "System Events" to tell process "Flycut"
	tell menu bar item 1 of menu bar 1
		click menu item "Clear All" of menu 0
	end tell
    delay 0.1
	click button "Clear" of window 1
end tell

