'setup-tags.vbs
'Give every label of the setup a tag and make every tag unique.
'
'  cscript //nologo setup-tags.vbs <setup> <summary>
'
'The two arguments are the same, in the same order, as
'setup-tags.applescript. Every path is absolute.
'
'One line goes to standard output: OK, or ERROR <number>: <text>.
'
'The setup wrapper leaves the workbook open, so its answer comes back whole
'and the summary is read through a second call. The workbook writes a summary
'of its own beside itself; the copy written here goes where R asked.
'
'The update rewrote the setup, so the workbook is saved before it closes. A
'script has nobody to press save.
'
'The script decides nothing. R builds every value and passes it in.

Option Explicit

Dim args
Dim setupPath, summaryPath
Dim excel, book, bookName
Dim answer, report
Dim failedNumber, failedText

Set args = WScript.Arguments

If args.Count < 2 Then
  WScript.Echo "ERROR 1: setup-tags.vbs takes 2 arguments."
  WScript.Quit 1
End If

setupPath = args(0)
summaryPath = args(1)

On Error Resume Next

Set excel = CreateObject("Excel.Application")
excel.Visible = True
excel.DisplayAlerts = False
excel.ScreenUpdating = False

Set book = excel.Workbooks.Open(setupPath)
bookName = book.Name

answer = excel.Run("'" & bookName & "'!RunSetupTags")
report = excel.Run("'" & bookName & "'!SetupLastSummary")

failedNumber = Err.Number
failedText = Err.Description
Err.Clear

book.Close True
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
