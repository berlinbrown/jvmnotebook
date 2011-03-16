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

import java.io.PrintWriter;

public class TraceClassVisitor extends TraceAbstractVisitor implements ClassVisitor
{

    /**
     * The {@link ClassVisitor} to which this visitor delegates calls. May be
     * <tt>null</tt>.
     */
    protected final ClassVisitor cv;

    /**
     * The print writer to be used to print the class.
     */
    protected final PrintWriter pw;
  
    /**
     * Constructs a new {@link TraceClassVisitor}.
     * 
     * @param pw the print writer to be used to print the class.
     */
    public TraceClassVisitor(final PrintWriter pw) {
        this(null, pw);
    }

    /**
     * Constructs a new {@link TraceClassVisitor}.
     * 
     * @param cv the {@link ClassVisitor} to which this visitor delegates calls.
     *        May be <tt>null</tt>.
     * @param pw the print writer to be used to print the class.
     */
    public TraceClassVisitor(final ClassVisitor cv, final PrintWriter pw) {
        this.cv = cv;
        this.pw = pw;
    }

    // ------------------------------------------------------------------------
    // Implementation of the ClassVisitor interface
    // ------------------------------------------------------------------------

    public void visit(
        final int version,
        final int access,
        final String name,
        final String signature,
        final String superName,
        final String[] interfaces)
    {
        int major = version & 0xFFFF;
        int minor = version >>> 16;
        buf.setLength(0);
        buf.append("// class version ")
                .append(major)
                .append('.')
                .append(minor)
                .append(" (")
                .append(version)
                .append(")\n");
        if ((access & Opcodes.ACC_DEPRECATED) != 0) {
            buf.append("// DEPRECATED\n");
        }
        buf.append("// access flags ").append(access).append('\n');

        appendDescriptor(CLASS_SIGNATURE, signature);
                
        appendAccess(access & ~Opcodes.ACC_SUPER);
        if ((access & Opcodes.ACC_ANNOTATION) != 0) {
            buf.append("@interface ");
        } else if ((access & Opcodes.ACC_INTERFACE) != 0) {
            buf.append("interface ");
        } else if ((access & Opcodes.ACC_ENUM) == 0) {
            buf.append("class ");
        }
        appendDescriptor(INTERNAL_NAME, name);

        if (superName != null && !"java/lang/Object".equals(superName)) {
            buf.append(" extends ");
            appendDescriptor(INTERNAL_NAME, superName);
            buf.append(' ');
        }
        if (interfaces != null && interfaces.length > 0) {
            buf.append(" implements ");
            for (int i = 0; i < interfaces.length; ++i) {
                appendDescriptor(INTERNAL_NAME, interfaces[i]);
                buf.append(' ');
            }
        }
        buf.append(" {\n\n");

        text.add(buf.toString());

        if (cv != null) {
            cv.visit(version, access, name, signature, superName, interfaces);
        }
    }

    public void visitSource(final String file, final String debug) {
        buf.setLength(0);
        if (file != null) {
            buf.append(tab)
                    .append("// compiled from: ")
                    .append(file)
                    .append('\n');
        }
        if (debug != null) {
            buf.append(tab)
                    .append("// debug info: ")
                    .append(debug)
                    .append('\n');
        }
        if (buf.length() > 0) {
            text.add(buf.toString());
        }

        if (cv != null) {
            cv.visitSource(file, debug);
        }
    }

    public void visitOuterClass(
        final String owner,
        final String name,
        final String desc)
    {
        buf.setLength(0);
        buf.append(tab).append("OUTERCLASS ");
        appendDescriptor(INTERNAL_NAME, owner);
        buf.append(' ');
        if (name != null) {
            buf.append(name).append(' ');
        }
        appendDescriptor(METHOD_DESCRIPTOR, desc);
        buf.append('\n');
        text.add(buf.toString());

        if (cv != null) {
            cv.visitOuterClass(owner, name, desc);
        }
    }
    
    public void visitAttribute(final Attribute attr) {
        text.add("\n");
        super.visitAttribute(attr);

        if (cv != null) {
            cv.visitAttribute(attr);
        }
    }

    public void visitInnerClass(
        final String name,
        final String outerName,
        final String innerName,
        final int access)
    {
        buf.setLength(0);
        buf.append(tab).append("// access flags ");
        buf.append(access & ~Opcodes.ACC_SUPER).append('\n');
        buf.append(tab);
        appendAccess(access);
        buf.append("INNERCLASS ");
        appendDescriptor(INTERNAL_NAME, name);
        buf.append(' ');
        appendDescriptor(INTERNAL_NAME, outerName);
        buf.append(' ');
        appendDescriptor(INTERNAL_NAME, innerName);
        buf.append('\n');
        text.add(buf.toString());

        if (cv != null) {
            cv.visitInnerClass(name, outerName, innerName, access);
        }
    }

    public FieldVisitor visitField(
        final int access,
        final String name,
        final String desc,
        final String signature,
        final Object value)
    {
        buf.setLength(0);
        buf.append('\n');
        if ((access & Opcodes.ACC_DEPRECATED) != 0) {
            buf.append(tab).append("// DEPRECATED\n");
        }
        buf.append(tab).append("// access flags ").append(access).append('\n');
                
        buf.append(tab);
        appendAccess(access);

        appendDescriptor(FIELD_DESCRIPTOR, desc);
        buf.append(' ').append(name);
        if (value != null) {
            buf.append(" = ");
            if (value instanceof String) {
                buf.append('\"').append(value).append('\"');
            } else {
                buf.append(value);
            }
        }

        buf.append('\n');
        text.add(buf.toString());       
        return null;
    }

    public MethodVisitor visitMethod(
        final int access,
        final String name,
        final String desc,
        final String signature,
        final String[] exceptions)
    {
        buf.setLength(0);
        buf.append('\n');
        if ((access & Opcodes.ACC_DEPRECATED) != 0) {
            buf.append(tab).append("// DEPRECATED\n");
        }
        buf.append(tab).append("// access flags ").append(access).append('\n');
       
        buf.append(tab);
        appendAccess(access);
        if ((access & Opcodes.ACC_NATIVE) != 0) {
            buf.append("native ");
        }
        if ((access & Opcodes.ACC_VARARGS) != 0) {
            buf.append("varargs ");
        }
        if ((access & Opcodes.ACC_BRIDGE) != 0) {
            buf.append("bridge ");
        }

        buf.append(name);
        appendDescriptor(METHOD_DESCRIPTOR, desc);
        if (exceptions != null && exceptions.length > 0) {
            buf.append(" throws ");
            for (int i = 0; i < exceptions.length; ++i) {
                appendDescriptor(INTERNAL_NAME, exceptions[i]);
                buf.append(' ');
            }
        }

        buf.append('\n');
        text.add(buf.toString());
        return null;
    }

    public void visitEnd() {
        text.add("}\n");

        print(pw);
        pw.flush();

        if (cv != null) {
            cv.visitEnd();
        }
    }

    // ------------------------------------------------------------------------
    // Utility methods
    // ------------------------------------------------------------------------
    

    /**
     * Appends a string representation of the given access modifiers to {@link
     * #buf buf}.
     * 
     * @param access some access modifiers.
     */
    private void appendAccess(final int access) {
        if ((access & Opcodes.ACC_PUBLIC) != 0) {
            buf.append("public ");
        }
        if ((access & Opcodes.ACC_PRIVATE) != 0) {
            buf.append("private ");
        }
        if ((access & Opcodes.ACC_PROTECTED) != 0) {
            buf.append("protected ");
        }
        if ((access & Opcodes.ACC_FINAL) != 0) {
            buf.append("final ");
        }
        if ((access & Opcodes.ACC_STATIC) != 0) {
            buf.append("static ");
        }
        if ((access & Opcodes.ACC_SYNCHRONIZED) != 0) {
            buf.append("synchronized ");
        }
        if ((access & Opcodes.ACC_VOLATILE) != 0) {
            buf.append("volatile ");
        }
        if ((access & Opcodes.ACC_TRANSIENT) != 0) {
            buf.append("transient ");
        }
        if ((access & Opcodes.ACC_ABSTRACT) != 0) {
            buf.append("abstract ");
        }
        if ((access & Opcodes.ACC_STRICT) != 0) {
            buf.append("strictfp ");
        }
        if ((access & Opcodes.ACC_ENUM) != 0) {
            buf.append("enum ");
        }
    }
}
