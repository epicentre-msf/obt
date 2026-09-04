-- designer-generate.applescript
-- Drive one linelist generation on a designer workbook.
--
--   osascript designer-generate.applescript <designer> <geo> <setup> <folder> <name>
--                                 <setup-language> <form-language> <ribbon>
--                                 <password> <debug-password>
--
-- The ten arguments are the same, in the same order, as designer-generate.vbs.
-- Every path is an absolute POSIX path. A geobase or a ribbon the run does
-- not use arrives as an empty string, and so does a password the linelist is
-- built without.
--
-- <password> is what the linelist opens with once it is saved.
-- <debug-password> is what its sheets and its structure are protected with.
-- The designer reads both off its Main sheet, and the run writes them there
-- with the rest.
--
-- One line goes to standard output: OK, or ERROR <number>: <text>.
--
-- The script decides nothing. R builds every value and passes it in.

on run argv
	if (count of argv) < 10 then
		return "ERROR 1: designer-generate.applescript takes 10 arguments."
	end if

	set designerPath to item 1 of argv
	set geoPath to item 2 of argv
	set setupPath to item 3 of argv
	set folderPath to item 4 of argv
	set linelistName to item 5 of argv
	set setupLanguage to item 6 of argv
	set formLanguage to item 7 of argv
	set ribbonPath to item 8 of argv
	set openPassword to item 9 of argv
	set debugPassword to item 10 of argv

	set bookName to my basename(designerPath)

	try
		tell application "Microsoft Excel"
			activate
			set display alerts to false

			open workbook workbook file name designerPath read only false

			tell sheet "Main" of workbook bookName
				set value of range "RNG_PathDico" to setupPath
				set value of range "RNG_PathGeo" to geoPath
				set value of range "RNG_LLDir" to folderPath
				set value of range "RNG_LLName" to linelistName
				set value of range "RNG_LangSetup" to setupLanguage
				set value of range "RNG_LLForm" to formLanguage
				set value of range "RNG_LLTemp" to ribbonPath
				set value of range "RNG_LLPwdOpen" to openPassword
				set value of range "RNG_LLPassword" to debugPassword
			end tell

			-- A generation on a large setup runs for many minutes. osascript
			-- gives up at 120 seconds without this, and the build dies with it.
			with timeout of 3600 seconds
				run VB macro (bookName & "!clickGenerate")
			end timeout

			close workbook bookName saving no
			quit saving no
		end tell
	on error errorText number errorNumber
		return "ERROR " & errorNumber & ": " & errorText
	end try

	return "OK"
end run

-- The file name of a path, read here so the script calls no shell.
on basename(wholePath)
	set AppleScript's text item delimiters to "/"
	set parts to text items of wholePath
	set AppleScript's text item delimiters to ""
	return item -1 of parts
end basename
