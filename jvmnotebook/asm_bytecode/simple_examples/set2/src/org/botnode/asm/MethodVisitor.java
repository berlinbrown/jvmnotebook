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

public interface MethodVisitor {

    // -------------------------------------------------------------------------
    // Annotations and non standard attributes
    // -------------------------------------------------------------------------

    /**
     * Visits a non standard attribute of this method.
     * 
     * @param attr an attribute.
     */
    void visitAttribute(Attribute attr);

    /**
     * Starts the visit of the method's code, if any (i.e. non abstract method).
     */
    void visitCode();

    void visitFrame(
        int type,
        int nLocal,
        Object[] local,
        int nStack,
        Object[] stack);

    // -------------------------------------------------------------------------
    // Normal instructions
    // -------------------------------------------------------------------------
   
    void visitInsn(int opcode);
   
    void visitIntInsn(int opcode, int operand);
   
    void visitVarInsn(int opcode, int var);

    void visitTypeInsn(int opcode, String type);

    void visitFieldInsn(int opcode, String owner, String name, String desc);
    
    void visitMethodInsn(int opcode, String owner, String name, String desc);
    
    void visitJumpInsn(int opcode, Label label);

    void visitLabel(Label label);

    // -------------------------------------------------------------------------
    // Special instructions
    // -------------------------------------------------------------------------

    /**
     * Visits a LDC instruction.
     * 
     * @param cst the constant to be loaded on the stack. This parameter must be
     *        a non null {@link Integer}, a {@link Float}, a {@link Long}, a
     *        {@link Double} a {@link String} (or a {@link Type} for
     *        <tt>.class</tt> constants, for classes whose version is 49.0 or
     *        more).
     */
    void visitLdcInsn(Object cst);

    /**
     * Visits an IINC instruction.
     * 
     * @param var index of the local variable to be incremented.
     * @param increment amount to increment the local variable by.
     */
    void visitIincInsn(int var, int increment);

    /**
     * Visits a TABLESWITCH instruction.
     * 
     * @param min the minimum key value.
     * @param max the maximum key value.
     * @param dflt beginning of the default handler block.
     * @param labels beginnings of the handler blocks. <tt>labels[i]</tt> is
     *        the beginning of the handler block for the <tt>min + i</tt> key.
     */
    void visitTableSwitchInsn(int min, int max, Label dflt, Label[] labels);

    /**
     * Visits a LOOKUPSWITCH instruction.
     * 
     * @param dflt beginning of the default handler block.
     * @param keys the values of the keys.
     * @param labels beginnings of the handler blocks. <tt>labels[i]</tt> is
     *        the beginning of the handler block for the <tt>keys[i]</tt> key.
     */
    void visitLookupSwitchInsn(Label dflt, int[] keys, Label[] labels);

    /**
     * Visits a MULTIANEWARRAY instruction.
     * 
     * @param desc an array type descriptor (see {@link Type Type}).
     * @param dims number of dimensions of the array to allocate.
     */
    void visitMultiANewArrayInsn(String desc, int dims);

    // -------------------------------------------------------------------------
    // Exceptions table entries, debug information, max stack and max locals
    // -------------------------------------------------------------------------

    /**
     * Visits a try catch block.
     * 
     * @param start beginning of the exception handler's scope (inclusive).
     * @param end end of the exception handler's scope (exclusive).
     * @param handler beginning of the exception handler's code.
     * @param type internal name of the type of exceptions handled by the
     *        handler, or <tt>null</tt> to catch any exceptions (for "finally"
     *        blocks).
     * @throws IllegalArgumentException if one of the labels has already been
     *         visited by this visitor (by the {@link #visitLabel visitLabel}
     *         method).
     */
    void visitTryCatchBlock(Label start, Label end, Label handler, String type);

    /**
     * Visits a local variable declaration.
     * 
     * @param name the name of a local variable.
     * @param desc the type descriptor of this local variable.
     * @param signature the type signature of this local variable. May be
     *        <tt>null</tt> if the local variable type does not use generic
     *        types.
     * @param start the first instruction corresponding to the scope of this
     *        local variable (inclusive).
     * @param end the last instruction corresponding to the scope of this local
     *        variable (exclusive).
     * @param index the local variable's index.
     * @throws IllegalArgumentException if one of the labels has not already
     *         been visited by this visitor (by the
     *         {@link #visitLabel visitLabel} method).
     */
    void visitLocalVariable(
        String name,
        String desc,
        String signature,
        Label start,
        Label end,
        int index);

    /**
     * Visits a line number declaration.
     * 
     * @param line a line number. This number refers to the source file from
     *        which the class was compiled.
     * @param start the first instruction corresponding to this line number.
     * @throws IllegalArgumentException if <tt>start</tt> has not already been
     *         visited by this visitor (by the {@link #visitLabel visitLabel}
     *         method).
     */
    void visitLineNumber(int line, Label start);

    /**
     * Visits the maximum stack size and the maximum number of local variables
     * of the method.
     * 
     * @param maxStack maximum stack size of the method.
     * @param maxLocals maximum number of local variables for the method.
     */
    void visitMaxs(int maxStack, int maxLocals);

    /**
     * Visits the end of the method. This method, which is the last one to be
     * called, is used to inform the visitor that all the annotations and
     * attributes of the method have been visited.
     */
    void visitEnd();
}
