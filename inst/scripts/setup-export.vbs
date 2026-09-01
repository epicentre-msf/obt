'setup-export.vbs
'Write the setup out as an .xlsx file.
'
'  cscript //nologo setup-export.vbs <setup> <folder> <summary>
'
'The three arguments are the same, in the same order, as
'setup-export.applescript. Every path is absolute.
'
'One line goes to standard output: OK, or ERROR <number>: <text>.
'
'The setup wrapper leaves the workbook open, so its answer comes back whole
'and the summary is read through a second call. The workbook writes a summary
'of its own under the name of the file it produced, and R cannot know that
'name; the copy written here goes where R asked.
'
'The export reads the setup and writes a file of its own, so the workbook
'closes with nothing saved.
'
'The script decides nothing. R builds every value and passes it in.

Option Explicit

Dim args
Dim setupPath, folderPath, summaryPath
Dim excel, book, bookName
Dim answer, report
Dim failedNumber, failedText

Set args = WScript.Arguments

If args.Count < 3 Then
  WScript.Echo "ERROR 1: setup-export.vbs takes 3 arguments."
  WScript.Quit 1
End If

setupPath = args(0)
folderPath = args(1)
summaryPath = args(2)

On Error Resume Next

Set excel = CreateObject("Excel.Application")
excel.Visible = True
excel.DisplayAlerts = False
excel.ScreenUpdating = False

Set book = excel.Workbooks.Open(setupPath)
bookName = book.Name

answer = excel.Run("'" & bookName & "'!RunSetupExport", folderPath)
report = excel.Run("'" & bookName & "'!SetupLastSummary")

failedNumber = Err.Number
failedText = Err.Description
Err.Clear

book.Close False
excel.Quit
Err.Clear

Set book = Nothing
Set excel = Nothing
Set args = Nothing

WriteSummary summaryPath, report
Err.Clear

If failedNumber <> 0 Then
  WScript.Echo "ERROR " & failedNumber & ": " & failedText
  WScript.Quit 1
End If

WScript.Echo answer

'The run, in the text the workbook answers. The transport loses the line this
'script prints on runs Excel finished green, and this file settles it.
Sub WriteSummary(path, text)
  Dim fso, stream

  If Len(path) = 0 Then Exit Sub
  If Len(text) = 0 Then Exit Sub

  Set fso = CreateObject("Scripting.FileSystemObject")
  Set stream = fso.CreateTextFile(path, True)
  stream.Write text
  stream.Close

  Set stream = Nothing
  Set fso = Nothing
End Sub
