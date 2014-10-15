# this code works, but there is a bug in hl-line-mode: it won't highlight it until eny key pressed
# do shell script "open -a Emacs && /Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait --eval '(ym-org-day-view)'"

tell application "Emacs"
	activate
	tell application "System Events"
		# F1 0x7a 122 -- get codes here: /System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h
		key code 122
	end tell
end tell
