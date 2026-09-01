-- linelist-geobase.applescript
-- Read a geobase into a generated linelist.
--
--   osascript linelist-geobase.applescript <linelist> <geobase>
--
-- The two arguments are the same, in the same order, as linelist-geobase.vbs.
-- Both paths are absolute POSIX paths.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>.
--
-- A RUN THAT WORKED ANSWERS NOBODY. The linelist saves itself and closes at
-- the end of the call, so the call it was made from raises where the workbook
-- has gone. That raise is swallowed here and nothing is printed. The linelist
-- wrote its summary beside the geobase before it closed, and R reads the run
-- back off that file.
--
-- A run that was refused answers its refusal and leaves the workbook open.
-- That answer is printed as it came.
--
-- A failure to open the workbook is printed, because there is no summary file
-- to read after one.
--
-- The script decides nothing. R builds every value and passes it in.

on run argv
	if (count of argv) < 2 then
		return "ERROR 1: linelist-geobase.applescript takes 2 arguments."
	end if

	set linelistPath to item 1 of argv
	set geoPath to item 2 of argv

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
			-- A geobase of a whole country runs for many minutes.
			with timeout of 3600 seconds
				set runAnswer to run VB macro (bookName & "!RunImportGeobase") arg1 geoPath
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
