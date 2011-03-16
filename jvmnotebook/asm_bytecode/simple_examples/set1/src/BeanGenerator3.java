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
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.PUTFIELD;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.V1_1;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.CheckClassAdapter;

/**
 * ASM Guide example class.
 * 
 * @author Eric Bruneton
 */
public class BeanGenerator3 extends ClassLoader {

    private String name;

    private Map<String, Type> fields;

    public BeanGenerator3(String name, Map<String, Type> fields) {
        this.name = name;
        this.fields = fields;
    }

    public void generate(ClassVisitor cv) {
        MethodVisitor mv;
        cv.visit(V1_1, ACC_PUBLIC, name, null, "java/lang/Object", null);

        // generate default constructor
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }

        Iterator<Map.Entry<String, Type>> i = fields.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry<String, Type> e = i.next();
            String fName = e.getKey();
            String fDesc = e.getValue().getDescriptor();
            Type fType = e.getValue();
            String mName;
            String mDesc;

            // generate field
            cv.visitField(ACC_PRIVATE, fName, fDesc, null, null).visitEnd();

            // generate getter
            mName = "get" + up(fName);
            mDesc = "()" + fDesc;
            mv = cv.visitMethod(ACC_PUBLIC, mName, mDesc, null, null);
            if (mv != null) {
                mv.visitCode();
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, name, fName, fDesc);
                mv.visitInsn(fType.getOpcode(IRETURN));
                mv.visitMaxs(fType.getSize(), 1);
                mv.visitEnd();
            }
            // generate setter
            mName = "set" + up(fName);
            mDesc = "(" + fDesc + ")V";
            mv = cv.visitMethod(ACC_PUBLIC, mName, mDesc, null, null);
            if (mv != null) {
                mv.visitCode();
                mv.visitVarInsn(ALOAD, 0);
                mv.visitVarInsn(fType.getOpcode(ILOAD), 1);
                mv.visitFieldInsn(PUTFIELD, name, fName, fDesc);
                mv.visitInsn(RETURN);
                mv.visitMaxs(1 + fType.getSize(), 1 + fType.getSize());
                mv.visitEnd();
            }
        }

        cv.visitEnd();
    }

    private static String up(String name) {
        return Character.toUpperCase(name.charAt(0)) + name.substring(1);
    }

    public static final void main(final String[] args) throws Exception {

        System.out.println("Running");

        Map<String, Type> fields = new HashMap<String, Type>();

        fields.put("f0", Type.BOOLEAN_TYPE);
        fields.put("f1", Type.INT_TYPE);
        fields.put("f2", Type.LONG_TYPE);
        fields.put("f3", Type.FLOAT_TYPE);
        fields.put("f4", Type.DOUBLE_TYPE);
        fields.put("f5", Type.getType("Ljava/lang/String;"));
        fields.put("f6", Type.getType("[I"));

        BeanGenerator3 cg = new BeanGenerator3("MyBean", fields);

        PrintWriter pw = new PrintWriter(System.out, true);
        ClassWriter cw = new ClassWriter(0);
        CheckClassAdapter ca = new CheckClassAdapter(cw);
        cg.generate(ca);
        final byte[] b = cw.toByteArray();
        Class c = cg.defineClass("MyBean", b, 0, b.length);
        Object bean = c.newInstance();
        
        final Class [] c2 = {};
        final java.lang.reflect.Method getF = c.getMethod("getF1", c2);
        Class[] classes1 = { int.class };
        final java.lang.reflect.Method setF = c.getMethod("setF1", classes1);
        Object[] args1 = { 999 };
        setF.invoke(bean, args1);

        // Inoke the getter.      
        final int val = ((Integer) getF.invoke(bean)).intValue();
        System.out.println("Running =>" + val);
    }
}
