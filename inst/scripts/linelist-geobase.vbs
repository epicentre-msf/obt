'linelist-geobase.vbs
'Read a geobase into a generated linelist.
'
'  cscript //nologo linelist-geobase.vbs <linelist> <password> <geobase>
'
'The three arguments are the same, in the same order, as
'linelist-geobase.applescript. Both paths are absolute. <password> is what
'the linelist opens with, and arrives as an empty string for a linelist built
'with none.
'
'One line goes to standard output: OK, or ERROR <number>: <text>.
'
'A RUN THAT WORKED ANSWERS NOBODY. The linelist saves itself and closes at the
'end of the call, so the call it was made from raises where the workbook has
'gone. That raise is swallowed here and nothing is printed. The linelist wrote
'its summary beside the geobase before it closed, and R reads the run back off
'that file.
'
'A run that was refused answers its refusal and leaves the workbook open. That
'answer is printed as it came.
'
'A failure to open the workbook is printed, because there is no summary file
'to read after one. A wrong password is what Excel says there.
'
'The script decides nothing. R builds every value and passes it in.

Option Explicit

Dim args
Dim linelistPath, openPassword, geoPath
Dim excel, book, bookName
Dim answer
Dim openedNumber, openedText

Set args = WScript.Arguments

If args.Count < 3 Then
  WScript.Echo "ERROR 1: linelist-geobase.vbs takes 3 arguments."
  WScript.Quit 1
End If

linelistPath = args(0)
openPassword = args(1)
geoPath = args(2)

On Error Resume Next

Set excel = CreateObject("Excel.Application")
excel.Visible = True
excel.DisplayAlerts = False
excel.ScreenUpdating = False

Set book = OpenWorkbook(excel, linelistPath, openPassword)
openedNumber = Err.Number
openedText = Err.Description
Err.Clear

If openedNumber <> 0 Then
  excel.Quit
  Err.Clear
  WScript.Echo "ERROR " & openedNumber & ": " & openedText
  WScript.Quit 1
End If

bookName = book.Name
Set book = Nothing

answer = excel.Run("'" & bookName & "'!RunImportGeobase", geoPath)
Err.Clear

excel.Quit
Err.Clear

Set excel = Nothing
Set args = Nothing

If Len(answer) > 0 Then
  WScript.Echo answer
End If

'Open the linelist with the password it was handed, an empty one included. A
'linelist that opens with none takes the empty password as no password. An
'open with no password at all puts up the password prompt on a protected
'linelist, and the prompt waits for a person. The empty password is handed
'over so that linelist answers error 1004 straight away.
Function OpenWorkbook(app, path, password)
  Set OpenWorkbook = app.Workbooks.Open(path, , , , password)
End Function
