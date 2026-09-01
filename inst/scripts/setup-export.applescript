-- setup-export.applescript
-- Write the setup out as an .xlsx file.
--
--   osascript setup-export.applescript <setup> <folder> <summary>
--
-- The three arguments are the same, in the same order, as setup-export.vbs.
-- Every path is an absolute POSIX path.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>.
--
-- The setup wrapper leaves the workbook open, so its answer comes back whole
-- and the summary is read through a second call. The workbook writes a
-- summary of its own under the name of the file it produced, and R cannot
-- know that name; the copy written here goes where R asked.
--
-- The export reads the setup and writes a file of its own, so the workbook
-- closes with nothing saved.
--
-- The script decides nothing. R builds every value and passes it in.

on run argv
	if (count of argv) < 3 then
		return "ERROR 1: setup-export.applescript takes 3 arguments."
	end if

	set setupPath to item 1 of argv
	set folderPath to item 2 of argv
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

			-- An export of a whole setup runs for minutes on a large one.
			with timeout of 3600 seconds
				set runAnswer to run VB macro (bookName & "!RunSetupExport") arg1 folderPath
			end timeout

			set runReport to run VB macro (bookName & "!SetupLastSummary")

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
