'quiet.vbs
'Read or write the open-time switch on one workbook.
'
'  cscript //nologo quiet.vbs <workbook> <password> <switch> <action> <value> <summary>
'
'The six arguments are the same, in the same order, as quiet.applescript.
'The workbook and the summary are absolute paths. <password> is what the
'workbook opens with, and arrives as an empty string for a workbook that
'opens with none. <action> is read or write, and on a read <value> arrives as
'an empty string.
'
'The switch is a workbook-level defined name. Its value is held as a formula,
'="Yes" or ="No", which is the shape the workbooks store a string in and the
'shape they read back. A workbook whose structure is protected still takes
'the name, so the open password is the one password this run needs.
'
'The workbook is opened with the events off, so the code a workbook runs while
'it opens stays out of the way. The boxes that code shows are what the switch
'is for, and a workbook that has not been told yet would show them here.
'
'One line goes to standard output: OK, or ERROR <number>: <text>. The value the
'workbook holds when the run ends goes to the summary file, and R reads it
'there.
'
'A failure to open the workbook is printed on its own, because a wrong
'password is what Excel says there and nothing after it can run.
'
'The script decides nothing. R builds every value and passes it in.

Option Explicit

'The action that writes. Every other value reads.
Const ACTION_WRITE = "write"

'What the summary says about how the run ended, and what it calls the value
'that was read back.
Const OUTCOME_LINE = "outcome=OK"
Const VALUE_KEY = "silent"

Dim args
Dim workbookPath, openPassword, switchName, actionName, switchValue, summaryPath
Dim excel, book, stored
Dim openedNumber, openedText
Dim failedNumber, failedText

Set args = WScript.Arguments

If args.Count < 6 Then
  WScript.Echo "ERROR 1: quiet.vbs takes 6 arguments."
  WScript.Quit 1
End If

workbookPath = args(0)
openPassword = args(1)
switchName = args(2)
actionName = args(3)
switchValue = args(4)
summaryPath = args(5)

stored = ""

On Error Resume Next

Set excel = CreateObject("Excel.Application")
excel.Visible = True
excel.DisplayAlerts = False
excel.ScreenUpdating = False
excel.EnableEvents = False

Set book = OpenWorkbook(excel, workbookPath, openPassword)
openedNumber = Err.Number
openedText = Err.Description
Err.Clear

If openedNumber <> 0 Then
  excel.Quit
  Err.Clear
  WScript.Echo "ERROR " & openedNumber & ": " & openedText
  WScript.Quit 1
End If

If actionName = ACTION_WRITE Then
  WriteSwitch book, switchName, switchValue
  book.Save
End If

'The write is asked about before the read runs, so a failed write is the
'answer the run gives. The read clears the error of a name that is not there,
'and it would clear this one too.
failedNumber = Err.Number
failedText = Err.Description
Err.Clear

If failedNumber = 0 Then
  stored = ReadSwitch(book, switchName)

  failedNumber = Err.Number
  failedText = Err.Description
  Err.Clear
End If

book.Close False
excel.EnableEvents = True
excel.Quit
Err.Clear

Set book = Nothing
Set excel = Nothing
Set args = Nothing

If failedNumber <> 0 Then
  WScript.Echo "ERROR " & failedNumber & ": " & failedText
  WScript.Quit 1
End If

WriteSummary summaryPath, stored

'The summary is the answer of a read, so a run that could not write one has
'nothing to report and says so.
If Err.Number <> 0 Then
  WScript.Echo "ERROR " & Err.Number & ": " & Err.Description
  WScript.Quit 1
End If

WScript.Echo "OK"

'Open the workbook with the password it was handed, an empty one included. A
'workbook that opens with none takes the empty password as no password. An
'open with no password at all puts up the password prompt on a protected
'workbook, and the prompt waits for a person. The empty password is handed
'over so that workbook answers error 1004 straight away.
Function OpenWorkbook(app, path, password)
  Set OpenWorkbook = app.Workbooks.Open(path, , , , password)
End Function

'Write the switch, on the name the workbook already carries or on one this
'run adds. A workbook built before the switch existed holds no such name.
Sub WriteSwitch(targetBook, nameId, wanted)
  Dim definition

  Set definition = Nothing

  On Error Resume Next
  Set definition = targetBook.Names(nameId)
  Err.Clear
  On Error GoTo 0

  If definition Is Nothing Then
    targetBook.Names.Add nameId, Wrap(wanted), False
  Else
    definition.RefersTo = Wrap(wanted)
    definition.Visible = False
  End If

  Set definition = Nothing
End Sub

'The value the workbook holds. A name that is not there answers an empty
'string, and R reads that as the switch being off.
Function ReadSwitch(targetBook, nameId)
  Dim definition

  ReadSwitch = ""
  Set definition = Nothing

  On Error Resume Next
  Set definition = targetBook.Names(nameId)
  Err.Clear
  On Error GoTo 0

  If definition Is Nothing Then Exit Function

  ReadSwitch = Unwrap(definition.RefersTo)

  Set definition = Nothing
End Function

'A string, as a defined name holds it.
Function Wrap(value)
  Wrap = "=""" & Replace(value, """", """""") & """"
End Function

'The string out of what a defined name holds.
Function Unwrap(formula)
  Dim body

  body = formula

  If Left(body, 1) = "=" Then body = Mid(body, 2)

  If Len(body) >= 2 Then
    If Left(body, 1) = """" And Right(body, 1) = """" Then
      body = Mid(body, 2, Len(body) - 2)
      body = Replace(body, """""", """")
    End If
  End If

  Unwrap = body
End Function

'What the run answers, beside the workbook it read. The transport loses the
'line this script prints on runs that finished, and this file settles it.
Sub WriteSummary(path, value)
  Dim files, stream

  Set files = CreateObject("Scripting.FileSystemObject")
  Set stream = files.CreateTextFile(path, True)

  stream.WriteLine OUTCOME_LINE
  stream.WriteLine VALUE_KEY & "=" & value
  stream.Close

  Set stream = Nothing
  Set files = Nothing
End Sub
