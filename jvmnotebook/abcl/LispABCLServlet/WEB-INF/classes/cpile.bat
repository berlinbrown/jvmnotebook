@echo off

REM #### Compile against the Tomcat servlet library
javac -classpath ..\..\compile-lib\servlet-api.jar;..\lib\abcl.jar LispExecute.java
