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

import static org.objectweb.asm.Opcodes.ACC_PRIVATE;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.ATHROW;
import static org.objectweb.asm.Opcodes.DUP;
import static org.objectweb.asm.Opcodes.F_SAME;
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.GOTO;
import static org.objectweb.asm.Opcodes.IADD;
import static org.objectweb.asm.Opcodes.ICONST_1;
import static org.objectweb.asm.Opcodes.IFLT;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.NEW;
import static org.objectweb.asm.Opcodes.PUTFIELD;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.V1_5;

import java.io.PrintWriter;
import java.lang.reflect.Method;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.util.TraceClassVisitor;

/**
 * ASM example class.
 */
public class Operations extends ClassLoader {

    public void buildConstantCalc(final MethodVisitor mv) {
        mv.visitCode();

        // visitVarInsn,
        // Visits a local variable instruction: ILOAD, LLOAD, FLOAD, DLOAD, ALOAD,
        //                     ISTORE, LSTORE, FSTORE, DSTORE, ASTORE, or RET.

        /*
         * From the asm guide:
         *
         * The first instruction pushes this on the operand stack, as before. The second
         * instruction pushes the local variable 1, which was initialized with the f argument
         * value during the creation of the frame for this method call. The third
         * instruction pops these two values and stores the int value in the f field of
         * the referenced object, i.e. in this.f. The last instruction, which is implicit
         * in the source code but which is mandatory in the compiled code, destroys the
         * current execution frame and returns to the caller.
         *
         * Example Setter:
         *
         * public void setF(int f) {
         *   this.f = f;
         * }
         *
         * ALOAD 0
         * ILOAD 1
         * PUTFIELD pkg/Bean f I
         * RETURN
         */

        // Push 'this' onto the stack
        mv.visitVarInsn(ALOAD, 0);

        // Add 1 + whatever F is on the stack
        mv.visitInsn(ICONST_1);
        mv.visitInsn(ICONST_1);

        mv.visitInsn(IADD);
        mv.visitFieldInsn(PUTFIELD, "pkg/Bean", "f", "I");

        // Return void
        mv.visitInsn(RETURN);

        // Visits the maximum stack size and the maximum number of local variables of the method.
        mv.visitMaxs(3, 1);
        mv.visitEnd();
    }


    /*
    --------------------------------------
    public testCalc_2b()V
     ALOAD 0
     DUP
     GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
     ALOAD 0
     GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
     IADD
     PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
     MAXSTACK = 3
     MAXLOCALS = 1

    --------------------------------------
    // access flags 1
    // Another example:
    public testCalc_2c()V
    L0
     LINENUMBER 80 L0
     ALOAD 0
     DUP
     GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
     ICONST_1
     IADD
     PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
    L1
     LINENUMBER 81 L1
     RETURN
    L2
     LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
     MAXSTACK = 3
     MAXLOCALS = 1
    --------------------------------------
     */
    public void buildFieldCalc(final MethodVisitor mv) {
        mv.visitCode();

        // Push 'this' onto the stack
        mv.visitVarInsn(ALOAD, 0);

        // Push 'this' onto the stack
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, "pkg/Bean", "f", "I");

        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, "pkg/Bean", "f", "I");
        mv.visitInsn(IADD);

        // 'this' is still on the stack
        mv.visitFieldInsn(PUTFIELD, "pkg/Bean", "f", "I");

        // Return void
        mv.visitInsn(RETURN);

        // Visits the maximum stack size and the maximum number of local variables of the method.
        mv.visitMaxs(3, 1);
        mv.visitEnd();
    }


    public byte[] generate(PrintWriter printWriter) {

        ClassWriter cw = new ClassWriter(0);
        ClassVisitor tcv = new TraceClassVisitor(cw, printWriter);
        ClassAdapter cv = new ClassAdapter(tcv);

        cv.visit(V1_5, ACC_PUBLIC, "pkg/Bean", null, "java/lang/Object", null);
        cv.visitSource("Bean.java", null);

        // Define a private field
        FieldVisitor fv = cv.visitField(ACC_PRIVATE, "f", "I", null, null);
        if (fv != null) {
            fv.visitEnd();
        }
        MethodVisitor mv;

        //*********************************
        // Build the object
        //*********************************
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>",
                    "()V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }

        //*********************************
        // Define the method 'get'
        //*********************************
        mv = cv.visitMethod(ACC_PUBLIC, "getF", "()I", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "pkg/Bean", "f", "I");
            mv.visitInsn(IRETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }

        //*********************************
        // Define the method 'set'
        //*********************************
        mv = cv.visitMethod(ACC_PUBLIC, "setF", "(I)V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ILOAD, 1);
            mv.visitFieldInsn(PUTFIELD, "pkg/Bean", "f", "I");
            mv.visitInsn(RETURN);
            mv.visitMaxs(2, 2);
            mv.visitEnd();
        }

        mv = cv.visitMethod(ACC_PUBLIC, "checkAndSetF", "(I)V", null, null);
        if (mv != null) {

            mv.visitCode();
            mv.visitVarInsn(ILOAD, 1);

            Label label = new Label();
            mv.visitJumpInsn(IFLT, label);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ILOAD, 1);
            mv.visitFieldInsn(PUTFIELD, "pkg/Bean", "f", "I");

            Label end = new Label();
            mv.visitJumpInsn(GOTO, end);
            mv.visitLabel(label);
            mv.visitFrame(F_SAME, 0, null, 0, null);
            mv.visitTypeInsn(NEW, "java/lang/IllegalArgumentException");
            mv.visitInsn(DUP);
            mv.visitMethodInsn(INVOKESPECIAL,
                    "java/lang/IllegalArgumentException", "<init>", "()V");
            mv.visitInsn(ATHROW);
            mv.visitLabel(end);
            mv.visitFrame(F_SAME, 0, null, 0, null);
            mv.visitInsn(RETURN);
            mv.visitMaxs(2, 2);
            mv.visitEnd();
        }

        //*******************************************
        // Create an add method
        //*******************************************
        mv = cv.visitMethod(ACC_PUBLIC, "calcConstant", "()V", null, null);
        if (mv != null) {
            buildConstantCalc(mv);
        }

        mv = cv.visitMethod(ACC_PUBLIC, "calcField", "()V", null, null);
        if (mv != null) {
            buildFieldCalc(mv);
        }

        // End of create class.
        cv.visitEnd();


        //*******************************************
        // Convert the data to a byte array
        //*******************************************
        return cw.toByteArray();
    }

    public static void main(final String[] args) throws Exception {

        System.out.println("Running");
        Operations cg = new Operations();
        PrintWriter pw = new PrintWriter(System.out, true);
        byte[] b = cg.generate(pw);
        Class c = cg.defineClass("pkg.Bean", b, 0, b.length);
        Object bean = c.newInstance();

        final Class c2[] = {};
        final Method getF = c.getMethod("getF", c2);
        final Method calc = c.getMethod("calcField", c2);

        Class[] classes1 = { int.class };
        Method setF = c.getMethod("setF", classes1);
        Object[] args1 = { 999 };
        setF.invoke(bean, args1);

        // Invoke the getter.
        final int val = ((Integer) getF.invoke(bean)).intValue();
        System.out.println("Running =>" + val);

        // Invoke the getter.
        final int val2 = ((Integer) getF.invoke(bean)).intValue();
        System.out.println("Running (2) =>" + val2);

        calc.invoke(bean);
        final int val3 = ((Integer) getF.invoke(bean)).intValue();
        System.out.println("Running (3) =>" + val3);

    }

}
