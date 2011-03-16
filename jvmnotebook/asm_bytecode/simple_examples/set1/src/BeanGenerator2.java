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
import static org.objectweb.asm.Opcodes.V1_5;

import java.io.PrintWriter;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.commons.Method;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

/**
 * ASM Guide example class.
 * 
 * @author Eric Bruneton
 */
public class BeanGenerator2 extends ClassLoader {

  public byte[] generate(PrintWriter printWriter) {
      
    ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    TraceClassVisitor tcv = new TraceClassVisitor(cw, printWriter);
    CheckClassAdapter cv = new CheckClassAdapter(tcv);
    cv.visit(V1_5, ACC_PUBLIC, "pkg/Bean", null, "java/lang/Object",
        null);
    cv.visitSource("Bean.java", null);
    FieldVisitor fv = cv.visitField(ACC_PRIVATE, "f", "I", null, null);
    if (fv != null) {
      fv.visitEnd();
    }
    MethodVisitor mv;
    mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
    if (mv != null) {
      Method m = Method.getMethod("void <init>()");
      GeneratorAdapter ga = new GeneratorAdapter(ACC_PUBLIC, m, mv);
      ga.visitCode();
      ga.loadThis();
      ga.invokeConstructor(Type.getType("Ljava/lang/Object;"), m);
      ga.returnValue();
      ga.endMethod();
    }
    mv = cv.visitMethod(ACC_PUBLIC, "getF", "()I", null, null);
    if (mv != null) {
      Type bean = Type.getType("Lpkg/Bean;");
      Method m = Method.getMethod("int getF()");
      GeneratorAdapter ga = new GeneratorAdapter(ACC_PUBLIC, m, mv);
      ga.visitCode();
      ga.loadThis();
      ga.getField(bean, "f", Type.INT_TYPE);
      ga.returnValue();
      ga.endMethod();
    }
    mv = cv.visitMethod(ACC_PUBLIC, "setF", "(I)V", null, null);
    if (mv != null) {
      Type bean = Type.getType("Lpkg/Bean;");
      Method m = Method.getMethod("void setF(int)");
      GeneratorAdapter ga = new GeneratorAdapter(ACC_PUBLIC, m, mv);
      ga.visitCode();
      ga.loadThis();
      ga.loadArg(0);
      ga.putField(bean, "f", Type.INT_TYPE);
      ga.returnValue();
      ga.endMethod();
    }
    mv = cv.visitMethod(ACC_PUBLIC, "checkAndSetF", "(I)V", null, null);
    if (mv != null) {
      Type bean = Type.getType("Lpkg/Bean;");
      Method m = Method.getMethod("void checkAndSetF(int)");
      GeneratorAdapter ga = new GeneratorAdapter(ACC_PUBLIC, m, mv);
      ga.visitCode();
      ga.loadArg(0);
      Label label = new Label();
      ga.ifZCmp(GeneratorAdapter.LT, label);
      ga.loadThis();
      ga.loadArg(0);
      ga.putField(bean, "f", Type.INT_TYPE);
      Label end = new Label();
      ga.goTo(end);
      ga.mark(label);
      ga.throwException(Type.getType(IllegalArgumentException.class),
          "f");
      ga.mark(end);
      ga.returnValue();
      ga.endMethod();
    }
    cv.visitEnd();
    byte[] b = cw.toByteArray();
    return b;
  }
  
  public static final void main(final String [] args) throws Exception {
      
      System.out.println("Running");
      
      BeanGenerator2 cg = new BeanGenerator2();
      PrintWriter pw = new PrintWriter(System.out, true);
      byte[] b = cg.generate(pw);                 
      Class c = cg.defineClass("pkg.Bean", b, 0, b.length);
      Object bean = c.newInstance();
      
      final Class c2 [] = {};
      final java.lang.reflect.Method getF = c.getMethod("getF", c2);
      Class [] classes1 = { int.class };
      final java.lang.reflect.Method setF = c.getMethod("setF", classes1 );
      Object [] args1 = { 999 };
      setF.invoke(bean, args1);
      
      // Inoke the getter.      
      final int val = ((Integer) getF.invoke(bean)).intValue();
      System.out.println("Running =>" + val);
      
      
  }
  
}
