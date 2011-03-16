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
 * @author bbrown
 */
public class MethodWriter {

    /**
     * Pseudo access flag used to denote constructors.
     */
    static final int ACC_CONSTRUCTOR = 262144;

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is zero.
     */
    static final int SAME_FRAME = 0; // to 63 (0-3f)

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is 1
     */
    static final int SAME_LOCALS_1_STACK_ITEM_FRAME = 64; // to 127 (40-7f)

    /**
     * Reserved for future use
     */
    static final int RESERVED = 128;

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is 1. Offset is bigger then 63;
     */
    static final int SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED = 247; // f7

    /**
     * Frame where current locals are the same as the locals in the previous
     * frame, except that the k last locals are absent. The value of k is given
     * by the formula 251-frame_type.
     */
    static final int CHOP_FRAME = 248; // to 250 (f8-fA)

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is zero. Offset is bigger then 63;
     */
    static final int SAME_FRAME_EXTENDED = 251; // fb

    /**
     * Frame where current locals are the same as the locals in the previous
     * frame, except that k additional locals are defined. The value of k is
     * given by the formula frame_type-251.
     */
    static final int APPEND_FRAME = 252; // to 254 // fc-fe

    /**
     * Full frame
     */
    static final int FULL_FRAME = 255; // ff

    /**
     * Indicates that the maximum stack size and number of local variables must
     * be automatically computed.
     */
    private static final int MAXS = 1;

    /**
     * Indicates that the stack map frames must be recomputed from scratch. In
     * this case the maximum stack size and number of local variables is also
     * recomputed from scratch.    
     */
    private static final int FRAMES = 0;
    private static final int NOTHING = 2;
    
    /**
     * The bytecode of this method.
     */
    private ByteVector code = new ByteVector();

    private int subroutines;

    private final String descriptor;

    private final int desc;

    private final int name;

    private int access;
           
    /**
     * Next method writer (see {@link ClassWriter#firstMethod firstMethod}).
     */
    MethodWriter next;

    /**
     * The class writer to which this method must be added.
     */
    final ClassWriter cw;

    private int maxStack;

    private int maxLocals;
    
    /**
     * Number of stack map frames in the StackMapTable attribute.
     */
    private int frameCount;
    
    private int maxStackSize;
    
    private int stackSize;
    
    private final int compute;
    
    private boolean resize;
    
    /**
     * The StackMapTable attribute.
     */
    private ByteVector stackMap;
    
    private int previousFrameOffset;
    
    private int[] previousFrame;
        
    private int frameIndex;
    
    /**
     * The current stack map frame. The first element contains the offset of the
     * instruction to which the frame corresponds, the second element is the
     * number of locals and the third one is the number of stack elements. The
     * local variables start at index 3 and are followed by the operand stack
     * values. In summary frame[0] = offset, frame[1] = nLocal, frame[2] =
     * nStack, frame[3] = nLocal. All types are encoded as integers, with the
     * same format as the one used in {@link Label}, but limited to BASE types.
     */
    private int[] frame;
    
    private int localVarCount;
    
    /**
     * The LocalVariableTable attribute.
     */
    private ByteVector localVar;

    /**
     * Number of entries in the LocalVariableTypeTable attribute.
     */
    private int localVarTypeCount;

    /**
     * The LocalVariableTypeTable attribute.
     */
    private ByteVector localVarType;
            
    //*****************************************************
    // Constructors and Methods
    //*****************************************************
    
    MethodWriter(final ClassWriter cw, final int access, final String name, final String desc, final String signature, final String[] exceptions,
            final boolean computeMaxs, final boolean computeFrames) {

        if (cw.firstMethod == null) {
            cw.firstMethod = this;
        } else {
            cw.lastMethod.next = this;
        }

        cw.lastMethod = this;
        this.cw = cw;
        this.access = access;
        this.name = cw.newUTF8(name);
        this.desc = cw.newUTF8(desc);
        this.descriptor = desc;

        this.compute = computeFrames ? FRAMES : (computeMaxs ? MAXS : NOTHING);
        if (computeMaxs || computeFrames) {
            if (computeFrames && "<init>".equals(name)) {
                this.access |= ACC_CONSTRUCTOR;
            }
            // updates maxLocals
            int size = getArgumentsAndReturnSizes(descriptor) >> 2;
            if ((access & Opcodes.ACC_STATIC) != 0) {
                --size;
            }
            maxLocals = size;
            // creates and visits the label for the first basic block
            labels = new Label();
            labels.status |= Label.PUSHED;
            visitLabel(labels);
        }
    }
    
    /**
     * Computes the size of the arguments and of the return value of a method.
     */
    static int getArgumentsAndReturnSizes(final String desc) {
        int n = 1;
        int c = 1;
        while (true) {
            char car = desc.charAt(c++);
            if (car == ')') {
                car = desc.charAt(c);
                return n << 2
                        | (car == 'V' ? 0 : (car == 'D' || car == 'J' ? 2 : 1));
            } else if (car == 'L') {
                while (desc.charAt(c++) != ';') {
                }
                n += 1;
            } else if (car == '[') {
                while ((car = desc.charAt(c)) == '[') {
                    ++c;
                }
                if (car == 'D' || car == 'J') {
                    n -= 1;
                }
            } else if (car == 'D' || car == 'J') {
                n += 2;
            } else {
                n += 1;
            }
        }
    }
    
    /**
     * Returns the size of the bytecode of this method.
     *
     */
    final int getSize() {
        
        int size = 8;
        if (code.length > 0) {
            cw.newUTF8("Code");
            size += 18 + code.length + 8 * handlerCount;
            if (localVar != null) {
                cw.newUTF8("LocalVariableTable");
                size += 8 + localVar.length;
            }
            if (localVarType != null) {
                cw.newUTF8("LocalVariableTypeTable");
                size += 8 + localVarType.length;
            }
                        
            if (stackMap != null) {
                boolean zip = (cw.version & 0xFFFF) >= Opcodes.V1_6;
                cw.newUTF8(zip ? "StackMapTable" : "StackMap");
                size += 8 + stackMap.length;
            }
            if (cattrs != null) {
                size += cattrs.getSize(cw, code.data, code.length, maxStack, maxLocals);
            }
        }
        if (attrs != null) {
            size += attrs.getSize(cw, null, 0, -1, -1);
        }
        return size;
    }

    /**
     * Puts the bytecode of this method in the given byte vector.
     */
    final void put(final ByteVector out) {
        out.putShort(access).putShort(name).putShort(desc);        
    }

    /**
     * Writes a short value in the given byte array.
     */
    static void writeShort(final byte[] b, final int index, final int s) {
        b[index] = (byte) (s >>> 8);
        b[index + 1] = (byte) s;
    }

} // End of class
