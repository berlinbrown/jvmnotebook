/*
 * AsmBuilder.java
 * Oct 16, 2008
 */
package org.botnode.asm;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;

/**
 * @author bbrown
 */
public class AsmBuilder {

    public static final void main(final String [] args) throws Exception {
        System.out.println("Asm Builder");

        final ClassWriter writer = new ClassWriter(0);
        // gets the bytecode of the Example class, and loads it dynamically
        final byte[] code = writer.toByteArray();
        final OutputStream stream = new FileOutputStream("Test.class");
        final BufferedOutputStream buf = new BufferedOutputStream(stream);
        try {
            buf.write(code, 0, code.length);
            buf.flush();
        } finally {
            buf.close();
        }

        System.out.println("Done, class written to disk.");

    } // End of the main

}
