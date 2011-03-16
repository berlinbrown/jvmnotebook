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
 * A label represents a position in the bytecode of a method. Labels are used
 * for jump, goto, and switch instructions, and for try catch blocks.
 * 
 * @author Eric Bruneton
 */
public class Label {

    static final int DEBUG = 1;
    static final int RESOLVED = 2;
    static final int RESIZED = 4;
    static final int PUSHED = 8;    
    static final int TARGET = 16;
    static final int STORE = 32;    
    static final int REACHABLE = 64;    
    static final int JSR = 128;
    static final int RET = 256;
    static final int SUBROUTINE = 512;
    static final int VISITED = 1024;
    
    public Object info;

    int status;

    int line;

    /**
     * The position of this label in the code, if known.
     */
    int position;

    /**
     * Number of forward references to this label, times two.
     */
    private int referenceCount;

    /**
     * Informations about forward references. Each forward reference is
     * described by two consecutive integers in this array.
     */
    private int[] srcAndRefPositions;

    // ------------------------------------------------------------------------

    
    /**
     * Start of the output stack relatively to the input stack. The exact
     * semantics of this field depends on the algorithm that is used.
     */
    int inputStackTop;

    /**
     * Maximum height reached by the output stack, relatively to the top of the
     * input stack. This maximum is always positive or null.
     */
    int outputStackMax;

    /**
     * Information about the input and output stack map frames of this basic
     * block. This field is only used when {@link ClassWriter#COMPUTE_FRAMES}
     * option is used.
     */
    Frame frame;

    /**
     * The successor of this label, in the order they are visited. This linked
     * list does not include labels used for debug info only. If
     * {@link ClassWriter#COMPUTE_FRAMES} option is used then, in addition, it
     * does not contain successive labels that denote the same bytecode position
     * (in this case only the first label appears in this list).
     */
    Label successor;

    /**
     * The successors of this node in the control flow graph. These successors
     * are stored in a linked list of {@link Edge Edge} objects, linked to each
     * other by their {@link Edge#next} field.
     */
    Edge successors;

    /**
     * The next basic block in the basic block stack. This stack is used in the
     * main loop of the fix point algorithm used in the second step of the
     * control flow analysis algorithms.
     * 
     * @see MethodWriter#visitMaxs
     */
    Label next;

    // ------------------------------------------------------------------------
    // Constructor
    // ------------------------------------------------------------------------

    /**
     * Constructs a new label.
     */
    public Label() {
    }

    // ------------------------------------------------------------------------
    // Methods to compute offsets and to manage forward references
    // ------------------------------------------------------------------------

    /**
     * Returns the offset corresponding to this label. This offset is computed
     * from the start of the method's bytecode. <i>This method is intended for
     * {@link Attribute} sub classes, and is normally not needed by class
     * generators or adapters.</i>
     */
    public int getOffset() {
        if ((status & RESOLVED) == 0) {
            throw new IllegalStateException("Label offset position has not been resolved yet");
        }
        return position;
    }

    /**
     * Puts a reference to this label in the bytecode of a method. If the
     * position of the label is known, the offset is computed and written
     * directly. Otherwise, a null offset is written and a new forward reference
     * is declared for this label.
     */
    void put(
        final MethodWriter owner,
        final ByteVector out,
        final int source,
        final boolean wideOffset)
    {
        if ((status & RESOLVED) == 0) {
            if (wideOffset) {
                addReference(-1 - source, out.length);
                out.putInt(-1);
            } else {
                addReference(source, out.length);
                out.putShort(-1);
            }
        } else {
            if (wideOffset) {
                out.putInt(position - source);
            } else {
                out.putShort(position - source);
            }
        }
    }

    /**
     * Adds a forward reference to this label. This method must be called only
     * for a true forward reference, i.e. only if this label is not resolved
     * yet. For backward references, the offset of the reference can be, and
     * must be, computed and stored directly.     
     */
    private void addReference(
        final int sourcePosition,
        final int referencePosition)
    {
        if (srcAndRefPositions == null) {
            srcAndRefPositions = new int[6];
        }
        if (referenceCount >= srcAndRefPositions.length) {
            int[] a = new int[srcAndRefPositions.length + 6];
            System.arraycopy(srcAndRefPositions,
                    0,
                    a,
                    0,
                    srcAndRefPositions.length);
            srcAndRefPositions = a;
        }
        srcAndRefPositions[referenceCount++] = sourcePosition;
        srcAndRefPositions[referenceCount++] = referencePosition;
    }

    /**
     * Resolves all forward references to this label. This method must be called
     * when this label is added to the bytecode of the method, i.e. when its
     * position becomes known. This method fills in the blanks that where left
     * in the bytecode by each forward reference previously added to this label.
     */
    boolean resolve(
        final MethodWriter owner,
        final int position,
        final byte[] data)
    {
        boolean needUpdate = false;
        this.status |= RESOLVED;
        this.position = position;
        int i = 0;
        while (i < referenceCount) {
            int source = srcAndRefPositions[i++];
            int reference = srcAndRefPositions[i++];
            int offset;
            if (source >= 0) {
                offset = position - source;
                if (offset < Short.MIN_VALUE || offset > Short.MAX_VALUE) {                                       
                    int opcode = data[reference - 1] & 0xFF;
                    if (opcode <= Opcodes.JSR) {
                        // changes IFEQ ... JSR to opcodes 202 to 217
                        data[reference - 1] = (byte) (opcode + 49);
                    } else {
                        // changes IFNULL and IFNONNULL to opcodes 218 and 219
                        data[reference - 1] = (byte) (opcode + 20);
                    }
                    needUpdate = true;
                }
                data[reference++] = (byte) (offset >>> 8);
                data[reference] = (byte) offset;
            } else {
                offset = position + source + 1;
                data[reference++] = (byte) (offset >>> 24);
                data[reference++] = (byte) (offset >>> 16);
                data[reference++] = (byte) (offset >>> 8);
                data[reference] = (byte) offset;
            }
        }
        return needUpdate;
    }

    /**
     * Returns the first label of the series to which this label belongs. For an
     * isolated label or for the first label in a series of successive labels,
     * this method returns the label itself. For other labels it returns the
     * first label of the series.
     * 
     * @return the first label of the series to which this label belongs.
     */
    Label getFirst() {
        return (frame == null) ? this : frame.owner;
    }

    // ------------------------------------------------------------------------
    // Methods related to subroutines
    // ------------------------------------------------------------------------

    /**
     * Returns true is this basic block belongs to the given subroutine.
     * 
     * @param id a subroutine id.
     * @return true is this basic block belongs to the given subroutine.
     */
    boolean inSubroutine(final long id) {
        if ((status & Label.VISITED) != 0) {
            return (srcAndRefPositions[(int) (id >>> 32)] & (int) id) != 0;
        }
        return false;
    }

    /**
     * Returns true if this basic block and the given one belong to a common
     * subroutine.
     * 
     * @param block another basic block.
     * @return true if this basic block and the given one belong to a common
     *         subroutine.
     */
    boolean inSameSubroutine(final Label block) {
        for (int i = 0; i < srcAndRefPositions.length; ++i) {
            if ((srcAndRefPositions[i] & block.srcAndRefPositions[i]) != 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Marks this basic block as belonging to the given subroutine.
     * 
     * @param id a subroutine id.
     * @param nbSubroutines the total number of subroutines in the method.
     */
    void addToSubroutine(final long id, final int nbSubroutines) {
        if ((status & VISITED) == 0) {
            status |= VISITED;
            srcAndRefPositions = new int[(nbSubroutines - 1) / 32 + 1];
        }
        srcAndRefPositions[(int) (id >>> 32)] |= (int) id;
    }
    
    /**
     * Finds the basic blocks that belong to a given subroutine, and marks these
     * blocks as belonging to this subroutine. This recursive method follows the
     * control flow graph to find all the blocks that are reachable from the
     * current block WITHOUT following any JSR target.     
     */
    void visitSubroutine(final Label JSR, final long id, final int nbSubroutines)
    {
        if (JSR != null) {
            if ((status & VISITED) != 0) {
                return;
            }
            status |= VISITED;
            // adds JSR to the successors of this block, if it is a RET block
            if ((status & RET) != 0) {
                if (!inSameSubroutine(JSR)) {
                    Edge e = new Edge();
                    e.info = inputStackTop;
                    e.successor = JSR.successors.successor;
                    e.next = successors;
                    successors = e;
                }
            }
        } else {
            // if this block already belongs to subroutine 'id', returns
            if (inSubroutine(id)) {
                return;
            }
            // marks this block as belonging to subroutine 'id'
            addToSubroutine(id, nbSubroutines);            
        }
        // calls this method recursively on each successor, except JSR targets
        Edge e = successors;
        while (e != null) {
            // if this block is a JSR block, then 'successors.next' leads
            // to the JSR target (see {@link #visitJumpInsn}) and must therefore
            // not be followed
            if ((status & Label.JSR) == 0 || e != successors.next) {
                e.successor.visitSubroutine(JSR, id, nbSubroutines);
            }
            e = e.next;
        }
    }

    // ------------------------------------------------------------------------
    // Overriden Object methods
    // ------------------------------------------------------------------------

    /**
     * Returns a string representation of this label.
     * 
     * @return a string representation of this label.
     */
    public String toString() {
        return "L" + System.identityHashCode(this);
    }
}
