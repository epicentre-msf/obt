'designer-generate.vbs
'Drive one linelist generation on a designer workbook.
'
'  cscript //nologo designer-generate.vbs <designer> <geo> <setup> <folder> <name>
'                                <setup-language> <form-language> <ribbon>
'                                <password> <debug-password>
'
'The ten arguments are the same, in the same order, as
'designer-generate.applescript. Every path is absolute. A geobase or a ribbon
'the run does not use arrives as an empty string, and so does a password the
'linelist is built without.
'
'<password> is what the linelist opens with once it is saved. <debug-password>
'is what its sheets and its structure are protected with. The designer reads
'both off its Main sheet, and the run writes them there with the rest.
'
'One line goes to standard output: OK, or ERROR <number>: <text>.
'
'The script decides nothing. R builds every value and passes it in.

Option Explicit

Dim args
Dim designerPath, geoPath, setupPath, folderPath, linelistName
Dim setupLanguage, formLanguage, ribbonPath, openPassword, debugPassword
Dim excel, book, sheet
Dim failedNumber, failedText

Set args = WScript.Arguments

If args.Count < 10 Then
  WScript.Echo "ERROR 1: designer-generate.vbs takes 10 arguments."
  WScript.Quit 1
End If

designerPath = args(0)
geoPath = args(1)
setupPath = args(2)
folderPath = args(3)
linelistName = args(4)
setupLanguage = args(5)
formLanguage = args(6)
ribbonPath = args(7)
openPassword = args(8)
debugPassword = args(9)

On Error Resume Next

Set excel = CreateObject("Excel.Application")
excel.Visible = True
excel.DisplayAlerts = False
excel.ScreenUpdating = False

Set book = excel.Workbooks.Open(designerPath)
Set sheet = book.Worksheets("Main")

sheet.Range("RNG_PathDico").Value = setupPath
sheet.Range("RNG_PathGeo").Value = geoPath
sheet.Range("RNG_LLDir").Value = folderPath
sheet.Range("RNG_LLName").Value = linelistName
sheet.Range("RNG_LangSetup").Value = setupLanguage
sheet.Range("RNG_LLForm").Value = formLanguage
sheet.Range("RNG_LLTemp").Value = ribbonPath
sheet.Range("RNG_LLPwdOpen").Value = openPassword
sheet.Range("RNG_LLPassword").Value = debugPassword

excel.Run "'" & book.Name & "'!clickGenerate"

failedNumber = Err.Number
failedText = Err.Description
Err.Clear

book.Close False
excel.Quit
Err.Clear

Set sheet = Nothing
Set book = Nothing
Set excel = Nothing
Set args = Nothing

If failedNumber <> 0 Then
  WScript.Echo "ERROR " & failedNumber & ": " & failedText
  WScript.Quit 1
End If

WScript.Echo "OK"
