#!/usr/bin/osascript

on run argv
    set cmd to item 1 of argv
    if cmd is equal to "clock_in"
        tell application "System Events" to set FrontAppName to name of first process where frontmost is true
        set emacs_active to ""
        if FrontAppName is "Emacs" then
            set emacs_active to " t"
        end if
        set task_name to do shell script ". ~/.profile ; $EMACSCLIENT --no-wait --eval '(ym-clock-in-and-notify" & emacs_active & ")' | xargs echo"
        display notification task_name with title "clock in"
        tell application "org-clock-statusbar" to clock in task_name
        do shell script "afplay ~/.emacs.d/org-clock-in.wav"
    else if cmd is equal to "clock_in_default"
        set task_name to do shell script ". ~/.profile ; $EMACSCLIENT --no-wait --eval '(ym-clock-in-default-and-notify)' | xargs echo"
        display notification task_name with title "clock in default"
        tell application "org-clock-statusbar" to clock in task_name
        do shell script "afplay ~/.emacs.d/org-clock-in.wav"
    else if cmd is equal to "clock_out"
        set task_name to do shell script ". ~/.profile ; $EMACSCLIENT --no-wait --eval '(ym-clock-out-and-notify)' | xargs echo"
        display notification "--- " & task_name with title "clock out"
        # display notification "--- " with title "clock out"
        tell application "org-clock-statusbar" to clock out
        do shell script "afplay ~/.emacs.d/org-clock-out.wav"
    else if cmd is equal to "show_current"
        do shell script ". ~/.profile ; $EMACSCLIENT --no-wait --eval '(ym-clock-show-current)'"
        tell application "Emacs"
        	activate
        end tell
    end if
end run








































