-- quiet.applescript
-- Read or write the open-time switch on one workbook.
--
--   osascript quiet.applescript <workbook> <password> <switch> <action> <value> <summary>
--
-- The six arguments are the same, in the same order, as quiet.vbs. The
-- workbook and the summary are absolute POSIX paths. <password> is what the
-- workbook opens with, and arrives as an empty string for a workbook that
-- opens with none. <action> is read or write, and on a read <value> arrives
-- as an empty string.
--
-- The switch is a workbook-level defined name, a named item here. Its value is
-- held as a formula, ="Yes" or ="No", which is the shape the workbooks store a
-- string in and the shape they read back. A workbook whose structure is
-- protected still takes the name, so the open password is the one password
-- this run needs.
--
-- The workbook is opened with the events off, so the code a workbook runs
-- while it opens stays out of the way. The boxes that code shows are what the
-- switch is for, and a workbook that has not been told yet would show them
-- here.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>. The value
-- the workbook holds when the run ends goes to the summary file, and R reads
-- it there.
--
-- A failure to open the workbook is printed on its own, because a wrong
-- password is what Excel says there and nothing after it can run.
--
-- The script decides nothing. R builds every value and passes it in.

-- The action that writes. Every other value reads.
property ACTION_WRITE : "write"

-- What the summary says about how the run ended, and what it calls the value
-- that was read back.
property OUTCOME_LINE : "outcome=OK"
property VALUE_KEY : "silent"

on run argv
	if (count of argv) < 6 then
		return "ERROR 1: quiet.applescript takes 6 arguments."
	end if

	set workbookPath to item 1 of argv
	set openPassword to item 2 of argv
	set switchName to item 3 of argv
	set actionName to item 4 of argv
	set switchValue to item 5 of argv
	set summaryPath to item 6 of argv

	set bookName to my basename(workbookPath)
	set stored to ""

	try
		tell application "Microsoft Excel"
			activate
			set display alerts to false
			set enable events to false
		end tell

		my openWorkbook(workbookPath, openPassword)

		tell application "Microsoft Excel"
			if actionName is ACTION_WRITE then
				my writeSwitch(bookName, switchName, my wrap(switchValue))
				save workbook bookName
			end if

			set stored to my readSwitch(bookName, switchName)

			close workbook bookName saving no
			set enable events to true
			quit saving no
		end tell
	on error errorText number errorNumber
		return "ERROR " & errorNumber & ": " & errorText
	end try

	try
		my writeSummary(summaryPath, stored)
	on error errorText number errorNumber
		return "ERROR " & errorNumber & ": " & errorText
	end try

	return "OK"
end run

-- Open the workbook with the password it was handed, an empty one included.
-- A workbook that opens with none takes the empty password as no password.
-- An open with no password at all puts up the password prompt on a protected
-- workbook, and the prompt waits for a person. The empty password is handed
-- over so that workbook answers a refusal straight away.
--
-- The open carries its own timeout. On the first run on a machine the grant
-- panel can put itself on the screen and wait for a person, and osascript
-- gives up at 120 seconds without this.
on openWorkbook(wholePath, openPassword)
	tell application "Microsoft Excel"
		with timeout of 600 seconds
			open workbook workbook file name wholePath password openPassword
		end timeout
	end tell
end openWorkbook

-- Write the switch, on the name the workbook already carries or on one this
-- run adds. A workbook built before the switch existed holds no such name.
on writeSwitch(bookName, switchName, wrapped)
	tell application "Microsoft Excel"
		try
			set references of named item switchName of workbook bookName to wrapped
		on error
			make new named item at workbook bookName with properties {name:switchName, references:wrapped}
		end try

		try
			set visible of named item switchName of workbook bookName to false
		end try
	end tell
end writeSwitch

-- The value the workbook holds. A name that is not there answers an empty
-- string, and R reads that as the switch being off.
on readSwitch(bookName, switchName)
	tell application "Microsoft Excel"
		try
			return my unwrap(references of named item switchName of workbook bookName)
		on error
			return ""
		end try
	end tell
end readSwitch

-- A string, as a defined name holds it.
on wrap(value)
	return "=\"" & my replace(value, "\"", "\"\"") & "\""
end wrap

-- The string out of what a defined name holds.
on unwrap(stored)
	set body to stored

	if body starts with "=" then set body to text 2 thru -1 of body

	if (length of body) ≥ 2 then
		if body starts with "\"" and body ends with "\"" then
			set body to text 2 thru -2 of body
			set body to my replace(body, "\"\"", "\"")
		end if
	end if

	return body
end unwrap

-- What the run answers, beside the workbook it read. The transport loses the
-- line this script prints on runs that finished, and this file settles it.
on writeSummary(summaryPath, value)
	set report to OUTCOME_LINE & linefeed & VALUE_KEY & "=" & value & linefeed
	set target to open for access (POSIX file summaryPath) with write permission

	try
		set eof of target to 0
		write report to target
		close access target
	on error errorText number errorNumber
		close access target
		error errorText number errorNumber
	end try
end writeSummary

-- One string swapped for another, read here so the script calls no shell.
on replace(whole, needle, wanted)
	set AppleScript's text item delimiters to needle
	set parts to text items of whole
	set AppleScript's text item delimiters to wanted
	set joined to parts as text
	set AppleScript's text item delimiters to ""
	return joined
end replace

-- The file name of a path, read here so the script calls no shell.
on basename(wholePath)
	set AppleScript's text item delimiters to "/"
	set parts to text items of wholePath
	set AppleScript's text item delimiters to ""
	return item -1 of parts
end basename
