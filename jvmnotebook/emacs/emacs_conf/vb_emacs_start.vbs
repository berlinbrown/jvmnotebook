'**************************************
' Simple script to launch Emacs
' (VB IDE is not required, should work with Windows XP)
' Berlin Brown - berlinbrown - berlin dot brown at gmail.com
' Created 8/20/2009
' Edited 1/11/2011
'
' Launch with:
' cscript SCRIPT_NAME.vbs
'
' See: http://www.emacswiki.org/cgi-bin/emacs-en/EmacsClient#toc3
'**************************************
Set objShell = WScript.CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

'**************************************
' Check if emacs is already running
'**************************************
strComputer = "."
Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\root\cimv2")
Set colItems = objWMIService.ExecQuery("Select * From Win32_Process")
Dim isRunning
isRunning = False  
For Each objItem in colItems
    If InStr(objItem.CommandLine, "emacs.exe") Then
      isRunning = True
    End If
Next
  
If WScript.Arguments.Count = 1 Then  
  If isRunning Then
    objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\emacsclientw.exe"" -n " & """" & WScript.Arguments(0) & """")
  Else
    objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\runemacs.exe"" " & """" & WScript.Arguments(0) & """")
  End If

Else
  ' ***** Print the Standard Out, the Usage ***********
  Wscript.StdOut.WriteLine "Running emacs startup (no arguments)"
  If isRunning Then
      objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\emacsclientw.exe"" -n C:/test.txt")
  Else
      objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\runemacs.exe"" ")
  End If

End If

