#!/bin/sh
# ANTLR startup script
# For use with cygwin
# Berlin Brown

LIB=`pwd | xargs cygpath -w`\\

LIB1=${LIB}..\\antlr-3.0.1.jar
LIB2=${LIB}..\\antlr-runtime-3.0.1.jar
LIB3=${LIB}..\\stringtemplate-3.1b1.jar
LIBb=${LIB}..\\antlr-2.7.7.jar

echo $LIB1
CP1=".;$LIBb;$LIB1;$LIB2;$LIB3"

java -classpath $CP1 org.antlr.Tool RemoteDef.g 
javac -classpath $CP1 *.java 
java -classpath $CP1 ParseMain ExampleRemoteDef.def 


