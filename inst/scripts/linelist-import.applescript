-- linelist-import.applescript
-- Read a file another linelist wrote for migration.
--
--   osascript linelist-import.applescript <linelist> <password> <file> <rule> <force>
--
-- The five arguments are the same, in the same order, as
-- linelist-import.vbs. Both paths are absolute POSIX paths. <password> is
-- what the linelist opens with, and arrives as an empty string for a linelist
-- built with none. <rule> is append or replace, and <force> is Yes or No.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>.
--
-- A RUN THAT WORKED ANSWERS NOBODY. The linelist saves itself and closes at
-- the end of the call, so the call it was made from raises where the workbook
-- has gone. That raise is swallowed here and nothing is printed. The linelist
-- wrote its summary beside the file it read before it closed, and R reads the
-- run back off that file.
--
-- A run that was refused answers its refusal and leaves the workbook open.
-- That answer is printed as it came, and the three warnings the linelist
-- raises about a file it cannot vouch for arrive that way.
--
-- A failure to open the workbook is printed, because there is no summary file
-- to read after one. A wrong password is what Excel says there.
--
-- The script decides nothing. R builds every value and passes it in.

on run argv
	if (count of argv) < 5 then
		return "ERROR 1: linelist-import.applescript takes 5 arguments."
	end if

	set linelistPath to item 1 of argv
	set openPassword to item 2 of argv
	set sourcePath to item 3 of argv
	set pastingRule to item 4 of argv
	set forceWord to item 5 of argv

	set bookName to my basename(linelistPath)
	set runAnswer to ""

	try
		tell application "Microsoft Excel"
			activate
			set display alerts to false
		end tell

		my openWorkbook(linelistPath, openPassword)
	on error errorText number errorNumber
		return "ERROR " & errorNumber & ": " & errorText
	end try

	try
		tell application "Microsoft Excel"
			-- A migration file of many thousands of rows runs for minutes.
			with timeout of 3600 seconds
				set runAnswer to run VB macro (bookName & "!RunImportData") arg1 sourcePath arg2 pastingRule arg3 forceWord
			end timeout
		end tell
	end try

	try
		tell application "Microsoft Excel" to quit saving no
	end try

	return runAnswer as text
end run

-- Open the linelist with the password it was handed, an empty one included.
-- A linelist that opens with none takes the empty password as no password.
-- An open with no password at all puts up the password prompt on a protected
-- linelist, and the prompt waits for a person. The empty password is handed
-- over so that linelist answers a refusal straight away.
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

-- The file name of a path, read here so the script calls no shell.
on basename(wholePath)
	set AppleScript's text item delimiters to "/"
	set parts to text items of wholePath
	set AppleScript's text item delimiters to ""
	return item -1 of parts
end basename
