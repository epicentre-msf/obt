-- setup-import.applescript
-- Read an .xlsx file into the working setup.
--
--   osascript setup-import.applescript <setup> <file> <summary>
--
-- The three arguments are the same, in the same order, as setup-import.vbs.
-- Every path is an absolute POSIX path.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>.
--
-- The setup wrapper leaves the workbook open, so its answer comes back whole
-- and the summary is read through a second call. The workbook writes a
-- summary of its own beside the file it read; the copy written here goes
-- where R asked.
--
-- The import rewrote the setup, so the workbook is saved before it closes. A
-- script has nobody to press save.
--
-- The script decides nothing. R builds every value and passes it in.

on run argv
	if (count of argv) < 3 then
		return "ERROR 1: setup-import.applescript takes 3 arguments."
	end if

	set setupPath to item 1 of argv
	set sourcePath to item 2 of argv
	set summaryPath to item 3 of argv

	set bookName to my basename(setupPath)
	set runAnswer to ""
	set runReport to ""

	try
		tell application "Microsoft Excel"
			activate
			set display alerts to false

			-- The open carries its own timeout. On the first run on a machine
			-- the grant panel can put itself on the screen and wait for a
			-- person, and osascript gives up at 120 seconds without this.
			with timeout of 600 seconds
				open workbook workbook file name setupPath
			end timeout

			-- An import of a whole setup runs for minutes on a large one.
			with timeout of 3600 seconds
				set runAnswer to run VB macro (bookName & "!RunSetupImportFile") arg1 sourcePath
			end timeout

			set runReport to run VB macro (bookName & "!SetupLastSummary")

			save workbook bookName
			close workbook bookName saving no
			quit saving no
		end tell
	on error errorText number errorNumber
		return "ERROR " & errorNumber & ": " & errorText
	end try

	try
		my writeSummary(summaryPath, runReport as text)
	end try

	return runAnswer as text
end run

-- The run, in the text the workbook answers. The transport loses the line
-- this script prints on runs Excel finished green, and this file settles it.
on writeSummary(summaryPath, runReport)
	if summaryPath is "" then return
	if runReport is "" then return

	set target to open for access (POSIX file summaryPath) with write permission

	try
		set eof of target to 0
		write runReport to target
		close access target
	on error errorText number errorNumber
		close access target
		error errorText number errorNumber
	end try
end writeSummary

-- The file name of a path, read here so the script calls no shell.
on basename(wholePath)
	set AppleScript's text item delimiters to "/"
	set parts to text items of wholePath
	set AppleScript's text item delimiters to ""
	return item -1 of parts
end basename
