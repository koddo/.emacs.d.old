#!/usr/bin/osascript

ignoring application responses
	tell application "System Events" to tell process "Jumpcut"
		click menu bar item 1 of menu bar 1
	end tell
end ignoring
do shell script "killall System\\ Events"
delay 0.1
tell application "System Events" to tell process "Jumpcut"
	tell menu bar item 1 of menu bar 1
		click menu item "Clear All" of menu 0
	end tell
	click button "Clear" of window 1
end tell
