package org.botnode.asm;

/***
 * ASM Guide
 * Copyright (c) 2007 Eric Bruneton
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

import java.io.PrintWriter;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.TraceClassVisitor;

/**
 * ASM example class.
 */
public class ReadClass extends ClassLoader {

    /**
     * Read a class from the classpath and then print with the following structure:
     *
Running:

// class version 50.0 (50)
// access flags 33
public class org/botnode/asm/ReadClass extends java/lang/ClassLoader  {

  // compiled from: ReadClass.java

  // access flags 1
  public <init>()V
   L0
    LINENUMBER 44 L0
    ALOAD 0
    INVOKESPECIAL java/lang/ClassLoader.<init> ()V
    RETURN
   L1
    LOCALVARIABLE this Lorg/botnode/asm/ReadClass; L0 L1 0
    MAXSTACK = 1
    MAXLOCALS = 1

  // access flags 9
  public static main([Ljava/lang/String;)V throws java/lang/Exception
   L0
    LINENUMBER 56 L0
    GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
    LDC "Running"
    INVOKEVIRTUAL java/io/PrintStream.println (Ljava/lang/String;)V
   L1
    LINENUMBER 58 L1
    NEW org/botnode/asm/ReadClass
    DUP
    INVOKESPECIAL org/botnode/asm/ReadClass.<init> ()V
    ASTORE 1
   L2
    LINENUMBER 59 L2
    NEW java/io/PrintWriter
    DUP
    GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
    ICONST_1
    INVOKESPECIAL java/io/PrintWriter.<init> (Ljava/io/OutputStream;Z)V
    ASTORE 2
   L3
    LINENUMBER 61 L3
    LDC "org.botnode.asm.ReadClass"
    INVOKESTATIC java/lang/Class.forName (Ljava/lang/String;)Ljava/lang/Class;
    ASTORE 3
   L4
    LINENUMBER 64 L4
    NEW org/objectweb/asm/ClassReader
    DUP
    LDC "org.botnode.asm.ReadClass"
    INVOKESPECIAL org/objectweb/asm/ClassReader.<init> (Ljava/lang/String;)V
    ASTORE 4
   L5
    LINENUMBER 65 L5
    NEW org/objectweb/asm/ClassWriter
    DUP
    ALOAD 4
    ICONST_0
    INVOKESPECIAL org/objectweb/asm/ClassWriter.<init> (Lorg/objectweb/asm/ClassReader;I)V
    ASTORE 5
   L6
    LINENUMBER 66 L6
    NEW org/objectweb/asm/util/TraceClassVisitor
    DUP
    ALOAD 5
    ALOAD 2
    INVOKESPECIAL org/objectweb/asm/util/TraceClassVisitor.<init> (Lorg/objectweb/asm/ClassVisitor;Ljava/io/PrintWriter;)V
    ASTORE 6
   L7
    LINENUMBER 67 L7
    NEW org/objectweb/asm/ClassAdapter
    DUP
    ALOAD 6
    INVOKESPECIAL org/objectweb/asm/ClassAdapter.<init> (Lorg/objectweb/asm/ClassVisitor;)V
    ASTORE 7
   L8
    LINENUMBER 68 L8
    ALOAD 4
    ALOAD 7
    ICONST_0
    INVOKEVIRTUAL org/objectweb/asm/ClassReader.accept (Lorg/objectweb/asm/ClassVisitor;I)V
   L9
    LINENUMBER 69 L9
    ALOAD 5
    INVOKEVIRTUAL org/objectweb/asm/ClassWriter.toByteArray ()[B
    POP
   L10
    LINENUMBER 70 L10
    RETURN
   L11
    LOCALVARIABLE args [Ljava/lang/String; L0 L11 0
    LOCALVARIABLE cg Lorg/botnode/asm/ReadClass; L2 L11 1
    LOCALVARIABLE pw Ljava/io/PrintWriter; L3 L11 2
    LOCALVARIABLE c Ljava/lang/Class; L4 L11 3
    LOCALVARIABLE cr Lorg/objectweb/asm/ClassReader; L5 L11 4
    LOCALVARIABLE cw Lorg/objectweb/asm/ClassWriter; L6 L11 5
    LOCALVARIABLE tcv Lorg/objectweb/asm/ClassVisitor; L7 L11 6
    LOCALVARIABLE cv Lorg/objectweb/asm/ClassAdapter; L8 L11 7
    MAXSTACK = 4
    MAXLOCALS = 8
}
     *
     * @param args
     * @throws Exception
     */
    public static void main(final String[] args) throws Exception {

        System.out.println("Running");

        ReadClass cg = new ReadClass();
        PrintWriter pw = new PrintWriter(System.out, true);

        Class c = Class.forName("org.botnode.asm.ReadClass");

        // Read a class and then write the data to system out.
        ClassReader cr = new ClassReader("org.botnode.asm.ReadClass");
        ClassWriter cw = new ClassWriter(cr, 0);
        ClassVisitor tcv = new TraceClassVisitor(cw, pw);
        ClassAdapter cv = new ClassAdapter(tcv);
        cr.accept(cv, 0);
        cw.toByteArray();
    }

}
