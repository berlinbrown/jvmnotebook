/**
 * 
 */
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.util.TraceMethodVisitor;

public class ExampleAsm extends ClassLoader implements Opcodes {

    //*****************************************************
        
    private TestClassLoader LOADER = new TestClassLoader();

    private static String getText(TraceMethodVisitor mv) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < mv.text.size(); i++) {
            sb.append(mv.text.get(i));
        }
        return sb.toString();
    }
    protected void generateBasicClass(ClassVisitor cv) {
        FieldVisitor fv;
        MethodVisitor mv;
        cv.visit(V1_1, ACC_PUBLIC, "C", null, "java/lang/Object", null);
        cv.visitSource("C.java", null);
        fv = cv.visitField(ACC_PUBLIC, "f", "I", null, null);
        if (fv != null) {
            fv.visitEnd();
        }
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        mv = cv.visitMethod(ACC_PUBLIC, "m", "()V", null, null);
        if (mv != null) {
            mv.visitCode();
            mv.visitLdcInsn(new Long(100));
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Thread", "sleep", "(J)V");
            mv.visitInsn(RETURN);
            mv.visitMaxs(3, 1);
            mv.visitEnd();
        }
        cv.visitEnd();
    }

    protected ClassVisitor getClassAdapter(ClassVisitor cv) {
        return null;
    }

    protected ClassNode generateBasicClass() {
        ClassNode cn = new ClassNode();
        generateBasicClass(cn);
        return cn;
    }

    protected Class defineClass(String name, byte[] b) {
        return LOADER.defineClass(name, b);
    }

    static class TestClassLoader extends ClassLoader {

        public Class defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
    
//  *****************************************************
    
    public static void main(final String[] args) throws Exception {

        // creates a ClassWriter for the Example public class,
        // which inherits from Object
        ClassWriter cw = new ClassWriter(0);
        cw.visit(V1_1, ACC_PUBLIC, "Example", null, "java/lang/Object", null);

        // creates a MethodWriter for the (implicit) constructor
        MethodVisitor mw = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);

        // pushes the 'this' variable
        mw.visitVarInsn(ALOAD, 0);

        // invokes the super class constructor
        mw.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
        mw.visitInsn(RETURN);

        // this code uses a maximum of one stack element and one local variable
        mw.visitMaxs(1, 1);
        mw.visitEnd();

        // creates a MethodWriter for the 'main' method
        mw = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        // pushes the 'out' field (of type PrintStream) of the System class
        mw.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");

        // pushes the "Hello World!" String constant
        mw.visitLdcInsn("This is only a test");
        // invokes the 'println' method (defined in the PrintStream class)
        mw.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
        mw.visitInsn(RETURN);
        // this code uses a maximum of two stack elements and two local
        // variables
        mw.visitMaxs(2, 2);
        mw.visitEnd();

        // gets the bytecode of the Example class, and loads it dynamically
        byte[] code = cw.toByteArray();

        // uses the dynamically generated class to print 'Helloworld'
        final ExampleAsm loader = new ExampleAsm();
        Class exampleClass = loader.defineClass("Example", code, 0, code.length);
        exampleClass.getMethods()[0].invoke(null, new Object[] { null });
    }

}
