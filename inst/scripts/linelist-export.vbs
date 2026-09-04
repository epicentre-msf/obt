'linelist-export.vbs
'Write an export out of a generated linelist.
'
'  cscript //nologo linelist-export.vbs <linelist> <password> <name> <folder>
'                                       <other-password> <other>
'
'The six arguments are the same, in the same order, as
'linelist-export.applescript. Every path is absolute. <password> is what the
'linelist opens with, and arrives as an empty string for a linelist built
'with none. <name> is the export to run, and an empty name asks for the
'migration export. <other> is the linelist to export from and
'<other-password> is what that file opens with; both empty means the linelist
'this call drives.
'
'One line goes to standard output: OK, or ERROR <number>: <text>.
'
'A RUN THAT WORKED ANSWERS NOBODY. The linelist closes at the end of the call,
'so the call it was made from raises where the workbook has gone. That raise
'is swallowed here and nothing is printed. The linelist wrote its summary into
'the output folder before it closed, and R reads the run back off that file.
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
Dim linelistPath, openPassword, exportName, folderPath, otherPassword, otherLinelist
Dim excel, book, bookName
Dim answer
Dim openedNumber, openedText

Set args = WScript.Arguments

If args.Count < 6 Then
  WScript.Echo "ERROR 1: linelist-export.vbs takes 6 arguments."
  WScript.Quit 1
End If

linelistPath = args(0)
openPassword = args(1)
exportName = args(2)
folderPath = args(3)
otherPassword = args(4)
otherLinelist = args(5)

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

answer = excel.Run("'" & bookName & "'!RunExport", exportName, folderPath, _
                   otherPassword, otherLinelist)
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
