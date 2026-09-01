-- linelist-export.applescript
-- Write an export out of a generated linelist.
--
--   osascript linelist-export.applescript <linelist> <name> <folder>
--                                         <password> <other>
--
-- The five arguments are the same, in the same order, as
-- linelist-export.vbs. Every path is an absolute POSIX path. <name> is the
-- export to run, and an empty name asks for the migration export. <other> is
-- the linelist to export from and <password> is what that file opens with;
-- both empty means the linelist this call drives.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>.
--
-- A RUN THAT WORKED ANSWERS NOBODY. The linelist closes at the end of the
-- call, so the call it was made from raises where the workbook has gone. That
-- raise is swallowed here and nothing is printed. The linelist wrote its
-- summary into the output folder before it closed, and R reads the run back
-- off that file.
--
-- A run that was refused answers its refusal and leaves the workbook open.
-- That answer is printed as it came.
--
-- A failure to open the workbook is printed, because there is no summary file
-- to read after one.
--
-- The script decides nothing. R builds every value and passes it in.

on run argv
	if (count of argv) < 5 then
		return "ERROR 1: linelist-export.applescript takes 5 arguments."
	end if

	set linelistPath to item 1 of argv
	set exportName to item 2 of argv
	set folderPath to item 3 of argv
	set otherPassword to item 4 of argv
	set otherLinelist to item 5 of argv

	set bookName to my basename(linelistPath)
	set runAnswer to ""

	try
		tell application "Microsoft Excel"
			activate
			set display alerts to false

			-- The open carries its own timeout. On the first run on a machine
			-- the grant panel can put itself on the screen and wait for a
			-- person, and osascript gives up at 120 seconds without this.
			with timeout of 600 seconds
				open workbook workbook file name linelistPath
			end timeout
		end tell
	on error errorText number errorNumber
		return "ERROR " & errorNumber & ": " & errorText
	end try

	try
		tell application "Microsoft Excel"
			-- An export walks the whole linelist and writes its files.
			with timeout of 3600 seconds
				set runAnswer to run VB macro (bookName & "!RunExport") arg1 exportName arg2 folderPath arg3 otherPassword arg4 otherLinelist
			end timeout
		end tell
	end try

	try
		tell application "Microsoft Excel" to quit saving no
	end try

	return runAnswer as text
end run

-- The file name of a path, read here so the script calls no shell.
on basename(wholePath)
	set AppleScript's text item delimiters to "/"
	set parts to text items of wholePath
	set AppleScript's text item delimiters to ""
	return item -1 of parts
end basename
