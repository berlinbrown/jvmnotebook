@echo off
REM ---- Application ---

REM ---- Clean the top directory --- 
del *.fas
del *.lib
del *.class
del *.php
del stream-run.bat

REM ---- Clean Codegen and Tests subs ---
del org\codegen\*.fas
del org\codegen\*.lib

del org\tests\*.fas
del org\tests\*.lib

del SFtpUpload.java
del FtpUpload.java

REM ---- End of File ---