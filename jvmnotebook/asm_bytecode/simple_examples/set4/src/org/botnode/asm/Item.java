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
 * A constant pool item. Constant pool items can be created with the 'newXXX'
 * methods in the {@link ClassWriter} class.
 * 
 * @author Eric Bruneton
 */
final class Item {

    /**
     * Index of this item in the constant pool.
     */
    int index;

    /**
     * Type of this constant pool item. A single class is used to represent all
     * constant pool item types, in order to minimize the bytecode size of this
     * package. The value of this field is one of {@link ClassWriter#INT},
     */
    int type;

    int intVal;

    long longVal;

    String strVal1;
    String strVal2;
   
    String strVal3;

    int hashCode;
    
    Item next;

    /**
     * Constructs an uninitialized {@link Item}.
     */
    Item() {
    }

    /**
     * Constructs an uninitialized {@link Item} for constant pool element at
     * given position.
     * 
     * @param index index of the item to be constructed.
     */
    Item(final int index) {
        this.index = index;
    }

    /**
     * Constructs a copy of the given item.
     *     
     */
    Item(final int index, final Item i) {
        this.index = index;
        type = i.type;
        intVal = i.intVal;
        longVal = i.longVal;
        strVal1 = i.strVal1;
        strVal2 = i.strVal2;
        strVal3 = i.strVal3;
        hashCode = i.hashCode;
    }

    /**
     * Sets this item to an integer item.
     * 
     * @param intVal the value of this item.
     */
    void set(final int intVal) {
        this.type = ClassWriter.INT;
        this.intVal = intVal;
        this.hashCode = 0x7FFFFFFF & (type + intVal);
    }

    /**
     * Sets this item to a long item.
     */
    void set(final long longVal) {
        this.type = ClassWriter.LONG;
        this.longVal = longVal;
        this.hashCode = 0x7FFFFFFF & (type + (int) longVal);
    }

    /**
     * Sets this item to a float item.
     */
    void set(final float floatVal) {
        this.type = ClassWriter.FLOAT;
        this.intVal = Float.floatToRawIntBits(floatVal);
        this.hashCode = 0x7FFFFFFF & (type + (int) floatVal);
    }

    /**
     * Sets this item to a double item.
     */
    void set(final double doubleVal) {
        this.type = ClassWriter.DOUBLE;
        this.longVal = Double.doubleToRawLongBits(doubleVal);
        this.hashCode = 0x7FFFFFFF & (type + (int) doubleVal);
    }

    /**
     * Sets this item to an item that do not hold a primitive value.
     */
    void set(
        final int type,
        final String strVal1,
        final String strVal2,
        final String strVal3)
    {
        this.type = type;
        this.strVal1 = strVal1;
        this.strVal2 = strVal2;
        this.strVal3 = strVal3;
        switch (type) {
            case ClassWriter.UTF8:
            case ClassWriter.STR:
            case ClassWriter.CLASS:
            case ClassWriter.TYPE_NORMAL:
                hashCode = 0x7FFFFFFF & (type + strVal1.hashCode());
                return;
            case ClassWriter.NAME_TYPE:
                hashCode = 0x7FFFFFFF & (type + strVal1.hashCode()
                        * strVal2.hashCode());
                return;
                // ClassWriter.FIELD:
                // ClassWriter.METH:
                // ClassWriter.IMETH:
            default:
                hashCode = 0x7FFFFFFF & (type + strVal1.hashCode()
                        * strVal2.hashCode() * strVal3.hashCode());
        }
    }

    /**
     * Indicates if the given item is equal to this one.
     * 
     */
    boolean isEqualTo(final Item i) {
        if (i.type == type) {
            switch (type) {
                case ClassWriter.INT:
                case ClassWriter.FLOAT:
                    return i.intVal == intVal;
                case ClassWriter.TYPE_MERGED:
                case ClassWriter.LONG:
                case ClassWriter.DOUBLE:
                    return i.longVal == longVal;
                case ClassWriter.UTF8:
                case ClassWriter.STR:
                case ClassWriter.CLASS:
                case ClassWriter.TYPE_NORMAL:
                    return i.strVal1.equals(strVal1);
                case ClassWriter.TYPE_UNINIT:
                    return i.intVal == intVal && i.strVal1.equals(strVal1);
                case ClassWriter.NAME_TYPE:
                    return i.strVal1.equals(strVal1)
                            && i.strVal2.equals(strVal2);
                    // ClassWriter.FIELD:
                    // ClassWriter.METH:
                    // ClassWriter.IMETH:
                default:
                    return i.strVal1.equals(strVal1)
                            && i.strVal2.equals(strVal2)
                            && i.strVal3.equals(strVal3);
            }
        }
        return false;
    }
}
