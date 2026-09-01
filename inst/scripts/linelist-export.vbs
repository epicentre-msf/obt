'linelist-export.vbs
'Write an export out of a generated linelist.
'
'  cscript //nologo linelist-export.vbs <linelist> <name> <folder>
'                                       <password> <other>
'
'The five arguments are the same, in the same order, as
'linelist-export.applescript. Every path is absolute. <name> is the export to
'run, and an empty name asks for the migration export. <other> is the linelist
'to export from and <password> is what that file opens with; both empty means
'the linelist this call drives.
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
'to read after one.
'
'The script decides nothing. R builds every value and passes it in.

Option Explicit

Dim args
Dim linelistPath, exportName, folderPath, otherPassword, otherLinelist
Dim excel, book, bookName
Dim answer
Dim openedNumber, openedText

Set args = WScript.Arguments

If args.Count < 5 Then
  WScript.Echo "ERROR 1: linelist-export.vbs takes 5 arguments."
  WScript.Quit 1
End If

linelistPath = args(0)
exportName = args(1)
folderPath = args(2)
otherPassword = args(3)
otherLinelist = args(4)

On Error Resume Next

Set excel = CreateObject("Excel.Application")
excel.Visible = True
excel.DisplayAlerts = False
excel.ScreenUpdating = False

Set book = excel.Workbooks.Open(linelistPath)
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
