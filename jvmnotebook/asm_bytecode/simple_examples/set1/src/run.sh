#!/bin/sh

LIB1=../lib/asm-3.1.jar
LIB2=../lib/asm-commons-3.1.jar
LIB3=../lib/asm-analysis-3.1.jar
LIB4=../lib/asm-commons-3.1.jar
LIB5=../lib/asm-util-3.1.jar

javac -classpath ".;$LIB1;$LIB2;$LIB3;$LIB4;$LIB5" BeanGenerator.java
javac -classpath ".;$LIB1;$LIB2;$LIB3;$LIB4;$LIB5" ExampleAsm.java


java -classpath ".;$LIB1;$LIB2;$LIB3;$LIB4;$LIB5" BeanGenerator
java -classpath ".;$LIB1;$LIB2;$LIB3;$LIB4;$LIB5" ExampleAsm 
