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
 */
public class Attribute {

    /**
     * The type of this attribute.
     */
    public final String type;

    byte[] value;

    Attribute next;

    /**
     * Constructs a new empty attribute.
     */
    protected Attribute(final String type) {
        this.type = type;
    }

    /**
     * Returns <tt>true</tt> if this type of attribute is unknown. The default
     * implementation of this method always returns <tt>true</tt>.
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
     * Returns the byte array form of this attribute.
     */
    protected ByteVector write(final ClassWriter cw, final byte[] code, final int len, final int maxStack,
            final int maxLocals) {
        ByteVector v = new ByteVector();
        v.data = value;
        v.length = value.length;
        return v;
    }

    /**
     * Returns the length of the attribute list that begins with this attribute.
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
     */
    final int getSize(final ClassWriter cw, final byte[] code, final int len, final int maxStack, final int maxLocals) {
        Attribute attr = this;
        int size = 0;

        while (attr != null) {
            cw.newUTF8(attr.type);
            size += attr.write(cw, code, len, maxStack, maxLocals).length + 6;
            attr = attr.next;
        }
        return size;
    }

    /**
     * Writes all the attributes of this attribute list in the given byte
     * vector.
     */
    final void put(final ClassWriter cw, final byte[] code, final int len, final int maxStack, final int maxLocals, final ByteVector out) {
        Attribute attr = this;
        while (attr != null) {
            ByteVector b = attr.write(cw, code, len, maxStack, maxLocals);
            out.putShort(cw.newUTF8(attr.type)).putInt(b.length);
            out.putByteArray(b.data, 0, b.length);
            attr = attr.next;
        }
    }
}
