/*
 * SimpleClassDecompile.java
 * Oct 15, 2008
 */
package org.botnode.asm;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Simple class that we will use to decompile.
 *
 * @author bbrown
 */
public class SimpleClassDecompile implements Serializable {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private String phoneNumber = "555-5555";
    public  String testPublic = "999";
    private long   testLong = 33;
    private double testDouble = 444;
    private int    testInteger = 44;
    private Integer theInteger = new Integer(333);
    private int    forSimpleCalc2b = 999;

    public static final void main(final String [] args) {

        System.out.println("Running");

        SimpleClassDecompile testObj = new SimpleClassDecompile();

        SimpleClassDecompile.utilMethod_0(888, 999);
        testObj.testCalc_2();
        testObj.testUtils_3("Str1", "Str2", "Str3");

        try {
            testObj.testCheckedExcept_5("a", "b", "c");
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("Done");

    }

    public static final int utilMethod_0(final int a, int b) {

        int abc_1 [] = { 1, 2, 3 };
        final String abc_2 [] = { "a", "b", "c" };

        System.out.println("a + b: " + a + b);

        System.out.println("arr check: " + abc_2[1]);
        return a + b;
    }

    public static class DogInnerClass_1 {
        private String testStr = "";

        public String getTestStr() {
            return testStr;
        }

        public void setTestStr(String testStr) {
            this.testStr = testStr;
        }

    }

    /*
        --------------------------------------
        public testCalc_2b()V
         ALOAD 0
         DUP
         GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
         ALOAD 0
         GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
         IADD
         PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
         MAXSTACK = 3
         MAXLOCALS = 1

        --------------------------------------
        // access flags 1
        // Another example:
        public testCalc_2c()V
        L0
         LINENUMBER 80 L0
         ALOAD 0
         DUP
         GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
         ICONST_1
         IADD
         PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
        L1
         LINENUMBER 81 L1
         RETURN
        L2
         LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
         MAXSTACK = 3
         MAXLOCALS = 1
        --------------------------------------
     */
    public void testCalc_2b() {
        forSimpleCalc2b = forSimpleCalc2b + forSimpleCalc2b;
    }
    public void testCalc_2c() {
        forSimpleCalc2b++;
    }

    public double testCalc_2() {

        final int f1 = 33;
        int f2 = 55;
        int res = 55 * 33;
        res++;

        for (int i = 0; i < 3; i++) {
            System.out.println("Run: " + i);
        }
        double g = 99.99;
        return g;
    }

    public List testUtils_3(String v1, String v2, String v3) {
        List list = new ArrayList();
        list.add("test");
        list.add(v1);
        list.add(v2);
        list.add(v3);

        for (Iterator it = list.iterator(); it.hasNext(); ) {
            System.out.println("Simple Iterator Test: " + it.next());
        }

        v1 = "Mutate the Local";

        return list;
    }

    public List testUncheckedException_4(final String v1, final String v2, final String v3) {
        if (true) {
            throw new RuntimeException("Just Testing Unchecked Exception");
        }
        return null;
    }
    public List testCheckedExcept_5(final String v1, final String v2, final String v3) throws Exception {
        if (true) {
            throw new Exception("Just Testing Checked Exception");
        }
        return null;
    }

    public String getPhoneNumber_6() {
        return phoneNumber;
    }


    public void setPhoneNumber_7(String phoneNumber) {
        this.phoneNumber = phoneNumber + "abc" + "123";
    }


    public String getTestPublic_8() {
        return testPublic;
    }


    public void setTestPublic_9(String testPublic) {
        this.testPublic = testPublic;
    }


    public long getTestLong_10() {
        return testLong;
    }


    public void setTestLong_11(long testLong) {
        this.testLong = testLong;
    }


    public double getTestDouble_12() {
        return testDouble;
    }


    public void setTestDouble_13(double testDouble) {
        this.testDouble = testDouble;
    }


    public int getTestInteger_14() {
        return testInteger;
    }


    public void setTestInteger_15(int testInteger) {
        this.testInteger = testInteger;
    }


    public Integer getTheInteger_16() {
        return theInteger;
    }


    public void setTheInteger_17(Integer theInteger) {
        this.theInteger = theInteger;
    }
}

/*
 * Full Expanded Class

//class version 50.0 (50)
//access flags 33
public class org/botnode/asm/SimpleClassDecompile implements java/io/Serializable  {

// compiled from: SimpleClassDecompile.java
// access flags 9
public static INNERCLASS org/botnode/asm/SimpleClassDecompile$DogInnerClass_1 org/botnode/asm/SimpleClassDecompile DogInnerClass_1

// access flags 26
private final static J serialVersionUID = 1

// access flags 2
private Ljava/lang/String; phoneNumber

// access flags 1
public Ljava/lang/String; testPublic

// access flags 2
private J testLong

// access flags 2
private D testDouble

// access flags 2
private I testInteger

// access flags 2
private Ljava/lang/Integer; theInteger

// access flags 2
private I forSimpleCalc2b

// access flags 1
public <init>()V
L0
 LINENUMBER 17 L0
 ALOAD 0
 INVOKESPECIAL java/lang/Object.<init>()V
L1
 LINENUMBER 24 L1
 ALOAD 0
 LDC "555-5555"
 PUTFIELD org/botnode/asm/SimpleClassDecompile.phoneNumber : Ljava/lang/String;
L2
 LINENUMBER 25 L2
 ALOAD 0
 LDC "999"
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testPublic : Ljava/lang/String;
L3
 LINENUMBER 26 L3
 ALOAD 0
 LDC 33
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testLong : J
L4
 LINENUMBER 27 L4
 ALOAD 0
 LDC 444.0
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testDouble : D
L5
 LINENUMBER 28 L5
 ALOAD 0
 BIPUSH 44
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testInteger : I
L6
 LINENUMBER 29 L6
 ALOAD 0
 NEW java/lang/Integer
 DUP
 SIPUSH 333
 INVOKESPECIAL java/lang/Integer.<init>(I)V
 PUTFIELD org/botnode/asm/SimpleClassDecompile.theInteger : Ljava/lang/Integer;
L7
 LINENUMBER 30 L7
 ALOAD 0
 SIPUSH 999
 PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
L8
 LINENUMBER 17 L8
 RETURN
L9
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L9 0
 MAXSTACK = 4
 MAXLOCALS = 1

// access flags 25
public final static main([Ljava/lang/String;)V
 TRYCATCHBLOCK L0 L1 L2 java/lang/Exception
L3
 LINENUMBER 34 L3
 GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
 LDC "Running"
 INVOKEVIRTUAL java/io/PrintStream.println(Ljava/lang/String;)V
L4
 LINENUMBER 36 L4
 NEW org/botnode/asm/SimpleClassDecompile
 DUP
 INVOKESPECIAL org/botnode/asm/SimpleClassDecompile.<init>()V
 ASTORE 1
L5
 LINENUMBER 38 L5
 SIPUSH 888
 SIPUSH 999
 INVOKESTATIC org/botnode/asm/SimpleClassDecompile.utilMethod_0(II)I
 POP
L6
 LINENUMBER 39 L6
 ALOAD 1
 INVOKEVIRTUAL org/botnode/asm/SimpleClassDecompile.testCalc_2()D
 POP2
L7
 LINENUMBER 40 L7
 ALOAD 1
 LDC "Str1"
 LDC "Str2"
 LDC "Str3"
 INVOKEVIRTUAL org/botnode/asm/SimpleClassDecompile.testUtils_3(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/util/List;
 POP
L0
 LINENUMBER 43 L0
 ALOAD 1
 LDC "a"
 LDC "b"
 LDC "c"
 INVOKEVIRTUAL org/botnode/asm/SimpleClassDecompile.testCheckedExcept_5(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/util/List;
 POP
L1
 GOTO L8
L2
 LINENUMBER 44 L2
 ASTORE 2
L9
 LINENUMBER 45 L9
 ALOAD 2
 INVOKEVIRTUAL java/lang/Exception.printStackTrace()V
L8
 LINENUMBER 49 L8
 GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
 LDC "Done"
 INVOKEVIRTUAL java/io/PrintStream.println(Ljava/lang/String;)V
L10
 LINENUMBER 51 L10
 RETURN
L11
 LOCALVARIABLE args [Ljava/lang/String; L3 L11 0
 LOCALVARIABLE testObj Lorg/botnode/asm/SimpleClassDecompile; L5 L11 1
 LOCALVARIABLE e Ljava/lang/Exception; L9 L8 2
 MAXSTACK = 4
 MAXLOCALS = 3

// access flags 25
public final static utilMethod_0(II)I
L0
 LINENUMBER 55 L0
 ICONST_3
 NEWARRAY T_INT
 DUP
 ICONST_0
 ICONST_1
 IASTORE
 DUP
 ICONST_1
 ICONST_2
 IASTORE
 DUP
 ICONST_2
 ICONST_3
 IASTORE
 ASTORE 2
L1
 LINENUMBER 56 L1
 ICONST_3
 ANEWARRAY java/lang/String
 DUP
 ICONST_0
 LDC "a"
 AASTORE
 DUP
 ICONST_1
 LDC "b"
 AASTORE
 DUP
 ICONST_2
 LDC "c"
 AASTORE
 ASTORE 3
L2
 LINENUMBER 58 L2
 GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
 NEW java/lang/StringBuilder
 DUP
 LDC "a + b: "
 INVOKESPECIAL java/lang/StringBuilder.<init>(Ljava/lang/String;)V
 ILOAD 0
 INVOKEVIRTUAL java/lang/StringBuilder.append(I)Ljava/lang/StringBuilder;
 ILOAD 1
 INVOKEVIRTUAL java/lang/StringBuilder.append(I)Ljava/lang/StringBuilder;
 INVOKEVIRTUAL java/lang/StringBuilder.toString()Ljava/lang/String;
 INVOKEVIRTUAL java/io/PrintStream.println(Ljava/lang/String;)V
L3
 LINENUMBER 60 L3
 GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
 NEW java/lang/StringBuilder
 DUP
 LDC "arr check: "
 INVOKESPECIAL java/lang/StringBuilder.<init>(Ljava/lang/String;)V
 ALOAD 3
 ICONST_1
 AALOAD
 INVOKEVIRTUAL java/lang/StringBuilder.append(Ljava/lang/String;)Ljava/lang/StringBuilder;
 INVOKEVIRTUAL java/lang/StringBuilder.toString()Ljava/lang/String;
 INVOKEVIRTUAL java/io/PrintStream.println(Ljava/lang/String;)V
L4
 LINENUMBER 61 L4
 ILOAD 0
 ILOAD 1
 IADD
 IRETURN
L5
 LOCALVARIABLE a I L0 L5 0
 LOCALVARIABLE b I L0 L5 1
 LOCALVARIABLE abc_1 [I L1 L5 2
 LOCALVARIABLE abc_2 [Ljava/lang/String; L2 L5 3
 MAXSTACK = 4
 MAXLOCALS = 4

// access flags 1
public testCalc_2b()V
L0
 LINENUMBER 77 L0
 ALOAD 0
 DUP
 GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
 IADD
 PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
L1
 LINENUMBER 78 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 MAXSTACK = 3
 MAXLOCALS = 1

// access flags 1
public testCalc_2c()V
L0
 LINENUMBER 80 L0
 ALOAD 0
 DUP
 GETFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
 ICONST_1
 IADD
 PUTFIELD org/botnode/asm/SimpleClassDecompile.forSimpleCalc2b : I
L1
 LINENUMBER 81 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 MAXSTACK = 3
 MAXLOCALS = 1

// access flags 1
public testCalc_2()D
L0
 LINENUMBER 85 L0
 BIPUSH 33
 ISTORE 1
L1
 LINENUMBER 86 L1
 BIPUSH 55
 ISTORE 2
L2
 LINENUMBER 87 L2
 SIPUSH 1815
 ISTORE 3
L3
 LINENUMBER 88 L3
 IINC 3 1
L4
 LINENUMBER 90 L4
 ICONST_0
 ISTORE 4
L5
 GOTO L6
L7
 LINENUMBER 91 L7
 GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
 NEW java/lang/StringBuilder
 DUP
 LDC "Run: "
 INVOKESPECIAL java/lang/StringBuilder.<init>(Ljava/lang/String;)V
 ILOAD 4
 INVOKEVIRTUAL java/lang/StringBuilder.append(I)Ljava/lang/StringBuilder;
 INVOKEVIRTUAL java/lang/StringBuilder.toString()Ljava/lang/String;
 INVOKEVIRTUAL java/io/PrintStream.println(Ljava/lang/String;)V
L8
 LINENUMBER 90 L8
 IINC 4 1
L6
 ILOAD 4
 ICONST_3
 IF_ICMPLT L7
L9
 LINENUMBER 93 L9
 LDC 99.99
 DSTORE 4
L10
 LINENUMBER 94 L10
 DLOAD 4
 DRETURN
L11
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L11 0
 LOCALVARIABLE f1 I L1 L11 1
 LOCALVARIABLE f2 I L2 L11 2
 LOCALVARIABLE res I L3 L11 3
 LOCALVARIABLE i I L5 L9 4
 LOCALVARIABLE g D L10 L11 4
 MAXSTACK = 4
 MAXLOCALS = 6

// access flags 1
public testUtils_3(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/util/List;
L0
 LINENUMBER 98 L0
 NEW java/util/ArrayList
 DUP
 INVOKESPECIAL java/util/ArrayList.<init>()V
 ASTORE 4
L1
 LINENUMBER 99 L1
 ALOAD 4
 LDC "test"
 INVOKEINTERFACE java/util/List.add(Ljava/lang/Object;)Z
 POP
L2
 LINENUMBER 100 L2
 ALOAD 4
 ALOAD 1
 INVOKEINTERFACE java/util/List.add(Ljava/lang/Object;)Z
 POP
L3
 LINENUMBER 101 L3
 ALOAD 4
 ALOAD 2
 INVOKEINTERFACE java/util/List.add(Ljava/lang/Object;)Z
 POP
L4
 LINENUMBER 102 L4
 ALOAD 4
 ALOAD 3
 INVOKEINTERFACE java/util/List.add(Ljava/lang/Object;)Z
 POP
L5
 LINENUMBER 104 L5
 ALOAD 4
 INVOKEINTERFACE java/util/List.iterator()Ljava/util/Iterator;
 ASTORE 5
L6
 GOTO L7
L8
 LINENUMBER 105 L8
 GETSTATIC java/lang/System.out : Ljava/io/PrintStream;
 NEW java/lang/StringBuilder
 DUP
 LDC "Simple Iterator Test: "
 INVOKESPECIAL java/lang/StringBuilder.<init>(Ljava/lang/String;)V
 ALOAD 5
 INVOKEINTERFACE java/util/Iterator.next()Ljava/lang/Object;
 INVOKEVIRTUAL java/lang/StringBuilder.append(Ljava/lang/Object;)Ljava/lang/StringBuilder;
 INVOKEVIRTUAL java/lang/StringBuilder.toString()Ljava/lang/String;
 INVOKEVIRTUAL java/io/PrintStream.println(Ljava/lang/String;)V
L7
 LINENUMBER 104 L7
 ALOAD 5
 INVOKEINTERFACE java/util/Iterator.hasNext()Z
 IFNE L8
L9
 LINENUMBER 108 L9
 LDC "Mutate the Local"
 ASTORE 1
L10
 LINENUMBER 110 L10
 ALOAD 4
 ARETURN
L11
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L11 0
 LOCALVARIABLE v1 Ljava/lang/String; L0 L11 1
 LOCALVARIABLE v2 Ljava/lang/String; L0 L11 2
 LOCALVARIABLE v3 Ljava/lang/String; L0 L11 3
 LOCALVARIABLE list Ljava/util/List; L1 L11 4
 LOCALVARIABLE it Ljava/util/Iterator; L6 L9 5
 MAXSTACK = 4
 MAXLOCALS = 6

// access flags 1
public testUncheckedException_4(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/util/List;
L0
 LINENUMBER 115 L0
 NEW java/lang/RuntimeException
 DUP
 LDC "Just Testing Unchecked Exception"
 INVOKESPECIAL java/lang/RuntimeException.<init>(Ljava/lang/String;)V
 ATHROW
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 LOCALVARIABLE v1 Ljava/lang/String; L0 L1 1
 LOCALVARIABLE v2 Ljava/lang/String; L0 L1 2
 LOCALVARIABLE v3 Ljava/lang/String; L0 L1 3
 MAXSTACK = 3
 MAXLOCALS = 4

// access flags 1
public testCheckedExcept_5(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/util/List; throws java/lang/Exception
L0
 LINENUMBER 121 L0
 NEW java/lang/Exception
 DUP
 LDC "Just Testing Checked Exception"
 INVOKESPECIAL java/lang/Exception.<init>(Ljava/lang/String;)V
 ATHROW
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 LOCALVARIABLE v1 Ljava/lang/String; L0 L1 1
 LOCALVARIABLE v2 Ljava/lang/String; L0 L1 2
 LOCALVARIABLE v3 Ljava/lang/String; L0 L1 3
 MAXSTACK = 3
 MAXLOCALS = 4

// access flags 1
public getPhoneNumber_6()Ljava/lang/String;
L0
 LINENUMBER 127 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.phoneNumber : Ljava/lang/String;
 ARETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 MAXSTACK = 1
 MAXLOCALS = 1

// access flags 1
public setPhoneNumber_7(Ljava/lang/String;)V
L0
 LINENUMBER 132 L0
 ALOAD 0
 NEW java/lang/StringBuilder
 DUP
 ALOAD 1
 INVOKESTATIC java/lang/String.valueOf(Ljava/lang/Object;)Ljava/lang/String;
 INVOKESPECIAL java/lang/StringBuilder.<init>(Ljava/lang/String;)V
 LDC "abc"
 INVOKEVIRTUAL java/lang/StringBuilder.append(Ljava/lang/String;)Ljava/lang/StringBuilder;
 LDC "123"
 INVOKEVIRTUAL java/lang/StringBuilder.append(Ljava/lang/String;)Ljava/lang/StringBuilder;
 INVOKEVIRTUAL java/lang/StringBuilder.toString()Ljava/lang/String;
 PUTFIELD org/botnode/asm/SimpleClassDecompile.phoneNumber : Ljava/lang/String;
L1
 LINENUMBER 133 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 LOCALVARIABLE phoneNumber Ljava/lang/String; L0 L2 1
 MAXSTACK = 4
 MAXLOCALS = 2

// access flags 1
public getTestPublic_8()Ljava/lang/String;
L0
 LINENUMBER 137 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.testPublic : Ljava/lang/String;
 ARETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 MAXSTACK = 1
 MAXLOCALS = 1

// access flags 1
public setTestPublic_9(Ljava/lang/String;)V
L0
 LINENUMBER 142 L0
 ALOAD 0
 ALOAD 1
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testPublic : Ljava/lang/String;
L1
 LINENUMBER 143 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 LOCALVARIABLE testPublic Ljava/lang/String; L0 L2 1
 MAXSTACK = 2
 MAXLOCALS = 2

// access flags 1
public getTestLong_10()J
L0
 LINENUMBER 147 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.testLong : J
 LRETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 MAXSTACK = 2
 MAXLOCALS = 1

// access flags 1
public setTestLong_11(J)V
L0
 LINENUMBER 152 L0
 ALOAD 0
 LLOAD 1
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testLong : J
L1
 LINENUMBER 153 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 LOCALVARIABLE testLong J L0 L2 1
 MAXSTACK = 3
 MAXLOCALS = 3

// access flags 1
public getTestDouble_12()D
L0
 LINENUMBER 157 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.testDouble : D
 DRETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 MAXSTACK = 2
 MAXLOCALS = 1

// access flags 1
public setTestDouble_13(D)V
L0
 LINENUMBER 162 L0
 ALOAD 0
 DLOAD 1
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testDouble : D
L1
 LINENUMBER 163 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 LOCALVARIABLE testDouble D L0 L2 1
 MAXSTACK = 3
 MAXLOCALS = 3

// access flags 1
public getTestInteger_14()I
L0
 LINENUMBER 167 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.testInteger : I
 IRETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 MAXSTACK = 1
 MAXLOCALS = 1

// access flags 1
public setTestInteger_15(I)V
L0
 LINENUMBER 172 L0
 ALOAD 0
 ILOAD 1
 PUTFIELD org/botnode/asm/SimpleClassDecompile.testInteger : I
L1
 LINENUMBER 173 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 LOCALVARIABLE testInteger I L0 L2 1
 MAXSTACK = 2
 MAXLOCALS = 2

// access flags 1
public getTheInteger_16()Ljava/lang/Integer;
L0
 LINENUMBER 177 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile.theInteger : Ljava/lang/Integer;
 ARETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L1 0
 MAXSTACK = 1
 MAXLOCALS = 1

// access flags 1
public setTheInteger_17(Ljava/lang/Integer;)V
L0
 LINENUMBER 182 L0
 ALOAD 0
 ALOAD 1
 PUTFIELD org/botnode/asm/SimpleClassDecompile.theInteger : Ljava/lang/Integer;
L1
 LINENUMBER 183 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile; L0 L2 0
 LOCALVARIABLE theInteger Ljava/lang/Integer; L0 L2 1
 MAXSTACK = 2
 MAXLOCALS = 2
}

*/

/*
//class version 50.0 (50)
//access flags 33
public class org/botnode/asm/SimpleClassDecompile$DogInnerClass_1 {

// compiled from: SimpleClassDecompile.java
// access flags 9
public static INNERCLASS org/botnode/asm/SimpleClassDecompile$DogInnerClass_1 org/botnode/asm/SimpleClassDecompile DogInnerClass_1

// access flags 2
private Ljava/lang/String; testStr

// access flags 1
public <init>()V
L0
 LINENUMBER 64 L0
 ALOAD 0
 INVOKESPECIAL java/lang/Object.<init>()V
L1
 LINENUMBER 65 L1
 ALOAD 0
 LDC ""
 PUTFIELD org/botnode/asm/SimpleClassDecompile$DogInnerClass_1.testStr : Ljava/lang/String;
L2
 LINENUMBER 64 L2
 RETURN
L3
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile$DogInnerClass_1; L0 L3 0
 MAXSTACK = 2
 MAXLOCALS = 1

// access flags 1
public getTestStr()Ljava/lang/String;
L0
 LINENUMBER 68 L0
 ALOAD 0
 GETFIELD org/botnode/asm/SimpleClassDecompile$DogInnerClass_1.testStr : Ljava/lang/String;
 ARETURN
L1
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile$DogInnerClass_1; L0 L1 0
 MAXSTACK = 1
 MAXLOCALS = 1

// access flags 1
public setTestStr(Ljava/lang/String;)V
L0
 LINENUMBER 72 L0
 ALOAD 0
 ALOAD 1
 PUTFIELD org/botnode/asm/SimpleClassDecompile$DogInnerClass_1.testStr : Ljava/lang/String;
L1
 LINENUMBER 73 L1
 RETURN
L2
 LOCALVARIABLE this Lorg/botnode/asm/SimpleClassDecompile$DogInnerClass_1; L0 L2 0
 LOCALVARIABLE testStr Ljava/lang/String; L0 L2 1
 MAXSTACK = 2
 MAXLOCALS = 2
}
*/