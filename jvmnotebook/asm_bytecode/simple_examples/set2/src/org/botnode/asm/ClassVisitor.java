/***
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2007 INRIA, France Telecom
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
package org.botnode.asm;

/**
 * A visitor to visit a Java class. The methods of this interface must be called
 * in the following order: <tt>visit</tt> [ <tt>visitSource</tt> ] [
 * <tt>visitOuterClass</tt> ] ( <tt>visitAnnotation</tt> |
 * <tt>visitAttribute</tt> )* (<tt>visitInnerClass</tt> |
 * <tt>visitField</tt> | <tt>visitMethod</tt> )* <tt>visitEnd</tt>.
 * 
 * @author Eric Bruneton
 */
public interface ClassVisitor {
   
    void visit(
        int version,
        int access,
        String name,
        String signature,
        String superName,
        String[] interfaces);

    /**
     * Visits the source of the class.
     * 
     * @param source the name of the source file from which the class was
     *        compiled. May be <tt>null</tt>.
     * @param debug additional debug information to compute the correspondance
     *        between source and compiled elements of the class. May be
     *        <tt>null</tt>.
     */
    void visitSource(String source, String debug);

    /**
     * Visits the enclosing class of the class. This method must be called only
     * if the class has an enclosing class.
     * 
     * @param owner internal name of the enclosing class of the class.
     * @param name the name of the method that contains the class, or
     *        <tt>null</tt> if the class is not enclosed in a method of its
     *        enclosing class.
     * @param desc the descriptor of the method that contains the class, or
     *        <tt>null</tt> if the class is not enclosed in a method of its
     *        enclosing class.
     */
    void visitOuterClass(String owner, String name, String desc);

    /**
     * Visits a non standard attribute of the class.
     * 
     * @param attr an attribute.
     */
    void visitAttribute(Attribute attr);
    
    void visitInnerClass(
        String name,
        String outerName,
        String innerName,
        int access);

    
    FieldVisitor visitField(
        int access,
        String name,
        String desc,
        String signature,
        Object value);
    
    MethodVisitor visitMethod(
        int access,
        String name,
        String desc,
        String signature,
        String[] exceptions);

  
    void visitEnd();
}
