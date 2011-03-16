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

import static org.objectweb.asm.Opcodes.ACC_INTERFACE;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ACC_STATIC;
import static org.objectweb.asm.Opcodes.ATHROW;
import static org.objectweb.asm.Opcodes.GETSTATIC;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.LADD;
import static org.objectweb.asm.Opcodes.LSUB;
import static org.objectweb.asm.Opcodes.PUTSTATIC;
import static org.objectweb.asm.Opcodes.RETURN;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceMethodVisitor;

import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.V1_1;

/**
 * ASM Guide example class.
 * 
 * @author Eric Bruneton
 */
public class AddTimerAdapter extends ClassAdapter {

    private String owner;

    private boolean isInterface;

    public AddTimerAdapter(ClassVisitor cv) {
        super(cv);
    }

    public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
        cv.visit(version, access, name, signature, superName, interfaces);
        owner = name;
        isInterface = (access & ACC_INTERFACE) != 0;
    }

    public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
        MethodVisitor mv = cv.visitMethod(access, name, desc, signature, exceptions);
        if (!isInterface && mv != null && !name.equals("<init>")) {
            mv = new AddTimerMethodAdapter(mv);
        }
        return mv;
    }

    public void visitEnd() {
        if (!isInterface) {
            FieldVisitor fv = cv.visitField(ACC_PUBLIC + ACC_STATIC, "timer", "J", null, null);
            if (fv != null) {
                fv.visitEnd();
            }
        }
        cv.visitEnd();
    }

    class AddTimerMethodAdapter extends MethodAdapter {

        public AddTimerMethodAdapter(MethodVisitor mv) {
            super(mv);
        }

        public void visitCode() {
            mv.visitCode();
            mv.visitFieldInsn(GETSTATIC, owner, "timer", "J");
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J");
            mv.visitInsn(LSUB);
            mv.visitFieldInsn(PUTSTATIC, owner, "timer", "J");
        }

        public void visitInsn(int opcode) {
            if ((opcode >= IRETURN && opcode <= RETURN) || opcode == ATHROW) {
                mv.visitFieldInsn(GETSTATIC, owner, "timer", "J");
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J");
                mv.visitInsn(LADD);
                mv.visitFieldInsn(PUTSTATIC, owner, "timer", "J");
            }
            mv.visitInsn(opcode);
        }

        public void visitMaxs(int maxStack, int maxLocals) {
            mv.visitMaxs(maxStack + 4, maxLocals);
        }
    }

    // *******************************************************
    
    private TestClassLoader LOADER = new TestClassLoader();

    private static String getText(TraceMethodVisitor mv) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < mv.text.size(); i++) {
            sb.append(mv.text.get(i));
        }
        return sb.toString();
    }

    protected void generateBasicClass(ClassVisitor cv) {
        FieldVisitor fv;
        MethodVisitor mv;
        cv.visit(V1_1, ACC_PUBLIC, "C", null, "java/lang/Object", null);
        cv.visitSource("C.java", null);
        fv = cv.visitField(ACC_PUBLIC, "f", "I", null, null);
        if (fv != null) {
            fv.visitEnd();
        }
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        mv = cv.visitMethod(ACC_PUBLIC, "m", "()V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitLdcInsn(new Long(100));
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Thread", "sleep", "(J)V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(3, 1);
            mv.visitEnd();
        }
        cv.visitEnd();
    }

    protected ClassNode generateBasicClass() {
        ClassNode cn = new ClassNode();
        generateBasicClass(cn);
        return cn;
    }

    protected Class defineClass(String name, byte[] b) {
        return LOADER.defineClass(name, b);
    }

    static class TestClassLoader extends ClassLoader {

        public Class defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }

    protected ClassVisitor getClassAdapter(ClassVisitor cv) {
        return new AddTimerAdapter(cv);
    }
        
    //*******************************************************

    public static final void main(final String[] args) throws Exception {

        System.out.println("Running");

        ClassWriter cw = new ClassWriter(0);
        CheckClassAdapter ca = new CheckClassAdapter(cw);
        
        AddTimerAdapter test = new AddTimerAdapter(null);        
        ClassVisitor cv = test.getClassAdapter(ca);
        test.generateBasicClass(cv);
        
        //************
        Class c = test.defineClass("C", cw.toByteArray());
        Object o = c.newInstance();
        
        // java.lang.reflect.Field;
        // java.lang.reflect.Method;
        
        final java.lang.reflect.Method m = c.getMethod("m");
        m.invoke(o);
        java.lang.reflect.Field f = c.getField("timer");
        System.out.println("Result: " + f.getLong(null));       
    }

}
