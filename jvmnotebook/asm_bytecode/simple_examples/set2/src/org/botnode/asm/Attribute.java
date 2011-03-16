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
 * A non standard class, field, method or code attribute.
 * 
 * @author Eric Bruneton
 * @author Eugene Kuleshov
 */
public class Attribute {

    /**
     * The type of this attribute.
     */
    public final String type;

    /**
     * The raw value of this attribute, used only for unknown attributes.
     */
    byte[] value;

    /**
     * The next attribute in this attribute list. May be <tt>null</tt>.
     */
    Attribute next;

    /**
     * Constructs a new empty attribute.
     * 
     * @param type the type of the attribute.
     */
    protected Attribute(final String type) {
        this.type = type;
    }

    /**
     * Returns <tt>true</tt> if this type of attribute is unknown. The default
     * implementation of this method always returns <tt>true</tt>.
     * 
     * @return <tt>true</tt> if this type of attribute is unknown.
     */
    public boolean isUnknown() {
        return true;
    }

    /**
     * Returns <tt>true</tt> if this type of attribute is a code attribute.
     * 
     * @return <tt>true</tt> if this type of attribute is a code attribute.
     */
    public boolean isCodeAttribute() {
        return false;
    }

    /**
     * Returns the labels corresponding to this attribute.
     * 
     * @return the labels corresponding to this attribute, or <tt>null</tt> if
     *         this attribute is not a code attribute that contains labels.
     */
    protected Label[] getLabels() {
        return null;
    }

   
    protected Attribute read(
        final ClassReader cr,
        final int off,
        final int len,
        final char[] buf,
        final int codeOff,
        final Label[] labels)
    {
        Attribute attr = new Attribute(type);
        attr.value = new byte[len];
        System.arraycopy(cr.b, off, attr.value, 0, len);
        return attr;
    }
   
    protected ByteVector write(
        final ClassWriter cw,
        final byte[] code,
        final int len,
        final int maxStack,
        final int maxLocals)
    {
        ByteVector v = new ByteVector();
        v.data = value;
        v.length = value.length;
        return v;
    }

    /**
     * Returns the length of the attribute list that begins with this attribute.
     * 
     * @return the length of the attribute list that begins with this attribute.
     */
    final int getCount() {
        int count = 0;
        Attribute attr = this;
        while (attr != null) {
            count += 1;
            attr = attr.next;
        }
        return count;
    }

    /**
     * Returns the size of all the attributes in this attribute list.
     * 
     * @param cw the class writer to be used to convert the attributes into byte
     *        arrays, with the {@link #write write} method.
     * @param code the bytecode of the method corresponding to these code
     *        attributes, or <tt>null</tt> if these attributes are not code
     *        attributes.
     * @param len the length of the bytecode of the method corresponding to
     *        these code attributes, or <tt>null</tt> if these attributes are
     *        not code attributes.
     * @param maxStack the maximum stack size of the method corresponding to
     *        these code attributes, or -1 if these attributes are not code
     *        attributes.
     * @param maxLocals the maximum number of local variables of the method
     *        corresponding to these code attributes, or -1 if these attributes
     *        are not code attributes.
     * @return the size of all the attributes in this attribute list. This size
     *         includes the size of the attribute headers.
     */
    final int getSize(
        final ClassWriter cw,
        final byte[] code,
        final int len,
        final int maxStack,
        final int maxLocals)
    {
        Attribute attr = this;
        int size = 0;
        while (attr != null) {
            cw.newUTF8(attr.type);
            size += attr.write(cw, code, len, maxStack, maxLocals).length + 6;
            attr = attr.next;
        }
        return size;
    }

    
    final void put(
        final ClassWriter cw,
        final byte[] code,
        final int len,
        final int maxStack,
        final int maxLocals,
        final ByteVector out)
    {
        Attribute attr = this;
        while (attr != null) {
            ByteVector b = attr.write(cw, code, len, maxStack, maxLocals);
            out.putShort(cw.newUTF8(attr.type)).putInt(b.length);
            out.putByteArray(b.data, 0, b.length);
            attr = attr.next;
        }
    }
}
