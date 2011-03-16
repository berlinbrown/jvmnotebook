/*
 * ClassWriter.java
 * Oct 16, 2008
 */
package org.botnode.asm;

/**
 * @author bbrown
 */
public class ClassWriter {

    /**
     * Minor and major version numbers of the class to be generated.
     */
    int version;

    public static final int COMPUTE_FRAMES = 2;

    public static final int COMPUTE_MAXS = 1;

    static final byte[] TYPE;

    static final int CLASS = 7;
    static final int FIELD = 9;
    static final int METH = 10;
    static final int IMETH = 11;
    static final int STR = 8;
    static final int INT = 3;
    static final int FLOAT = 4;
    static final int LONG = 5;
    static final int DOUBLE = 6;
    static final int NAME_TYPE = 12;
    static final int UTF8 = 1;
    static final int TYPE_NORMAL = 13;
    static final int TYPE_UNINIT = 14;
    static final int TYPE_MERGED = 15;

    private int access;


    /**
     * Index of the next item to be added in the constant pool.
     */
    int index;

    /**
     * A reusable key used to look for items in the {@link #items} hash table.
     */
    final Item key;
    final Item key2;
    final Item key3;

    /**
     * The constant pool's hash table data.
     */
    Item[] items;

    ByteVector pool;

    // Constant Pool Item for the name of the class.
    private int name;

    private int superName;

    /**
     * The internal name of this class.
     */
    String thisName;

    /**
     * This array contains the indexes of the constant pool items
     * that contain the internal names of these interfaces.
     */
    private int[] interfaces;

    private int interfaceCount = 0;

    int threshold;

    private final boolean computeMaxs;

    private final boolean computeFrames;

    boolean invalidFrames;

    FieldWriter firstField;

    FieldWriter lastField;

    MethodWriter firstMethod;

    MethodWriter lastMethod;

    /**
     * A type table used to temporarily store internal names that will not
     * necessarily be stored in the constant pool.
     */
    Item[] typeTable;

    /**
     * Number of elements in the {@link #typeTable} array.
     */
    private short typeCount;

    /**
     * The non standard attributes of this class.
     */
    private Attribute attrs;

    // ------------------------------------------------------------------------
    // Static initializer
    // ------------------------------------------------------------------------

    /**
     * Computes the instruction types of JVM opcodes.
     */
    static {
        int i;
        byte[] b = new byte[220];
        String s = "AAAAAAAAAAAAAAAABCKLLDDDDDEEEEEEEEEEEEEEEEEEEEAAAAAAAADD"
                +  "DDDEEEEEEEEEEEEEEEEEEEEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                +  "AAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAIIIIIIIIIIIIIIIIDNOAA"
                +  "AAAAGGGGGGGHAFBFAAFFAAQPIIJJIIIIIIIIIIIIIIIIII";
        for (i = 0; i < b.length; ++i) {
            b[i] = (byte) (s.charAt(i) - 'A');
        }
        TYPE = b;

    }

    // ------------------------------------------------------------------------
    // Constructor
    // ------------------------------------------------------------------------

    public ClassWriter(final int flags) {
        index = 1;
        pool = new ByteVector();
        items = new Item[256];
        threshold = (int) (0.75d * items.length);
        key = new Item();
        key2 = new Item();
        key3 = new Item();
        this.computeMaxs = (flags & COMPUTE_MAXS) != 0;
        this.computeFrames = (flags & COMPUTE_FRAMES) != 0;
    }

    // ------------------------------------------------------------------------
    // New Variable Definitions.
    // ------------------------------------------------------------------------


    /**
     * Adds the given internal name to {@link #typeTable} and returns its index.
     * Does nothing if the type table already contains this internal name.

     */
    int addType(final String type) {
        key.set(TYPE_NORMAL, type, null, null);
        Item result = get(key);
        if (result == null) {
            result = addType(key);
        }
        return result.index;
    }

    /**
     * Adds the given Item to {@link #typeTable}.
     */
    private Item addType(final Item item) {
        ++typeCount;
        Item result = new Item(typeCount, key);
        put(result);
        if (typeTable == null) {
            typeTable = new Item[16];
        }
        if (typeCount == typeTable.length) {
            Item[] newTable = new Item[2 * typeTable.length];
            System.arraycopy(typeTable, 0, newTable, 0, typeTable.length);
            typeTable = newTable;
        }
        typeTable[typeCount] = result;
        return result;
    }

    /**
     * Adds the given "uninitialized" type to {@link #typeTable} and returns its
     * index. This method is used for UNINITIALIZED types, made of an internal
     * name and a bytecode offset.
     */
    int addUninitializedType(final String type, final int offset) {
        key.type = TYPE_UNINIT;
        key.intVal = offset;
        key.strVal1 = type;
        key.hashCode = 0x7FFFFFFF & (TYPE_UNINIT + type.hashCode() + offset);
        Item result = get(key);
        if (result == null) {
            result = addType(key);
        }
        return result.index;
    }


    /**
     * Returns the index of the common super type of the two given types. This
     * method calls {@link #getCommonSuperClass} and caches the result in the
     * {@link #items} hash table to speedup future calls with the same
     * parameters.
     */
    int getMergedType(final int type1, final int type2) {
        key2.type = TYPE_MERGED;
        key2.longVal = type1 | (((long) type2) << 32);
        key2.hashCode = 0x7FFFFFFF & (TYPE_MERGED + type1 + type2);
        Item result = get(key2);
        if (result == null) {
            String t = typeTable[type1].strVal1;
            String u = typeTable[type2].strVal1;
            key2.intVal = addType(getCommonSuperClass(t, u));
            result = new Item((short) 0, key2);
            put(result);
        }
        return result.intVal;
    }

    /**
     * Returns the common super type of the two given types. The default
     * implementation of this method <i>loads<i> the two given classes and uses
     * the java.lang.Class methods to find the common super class.
     */
    protected String getCommonSuperClass(final String type1, final String type2) {
        Class c, d;
        try {
            c = Class.forName(type1.replace('/', '.'));
            d = Class.forName(type2.replace('/', '.'));
        } catch (Exception e) {
            throw new RuntimeException(e.toString());
        }
        if (c.isAssignableFrom(d)) {
            return type1;
        }
        if (d.isAssignableFrom(c)) {
            return type2;
        }
        if (c.isInterface() || d.isInterface()) {
            return "java/lang/Object";
        } else {
            do {
                c = c.getSuperclass();
            } while (!c.isAssignableFrom(d));
            return c.getName().replace('.', '/');
        }
    }

    /**
     * Adds an UTF8 string to the constant pool of the class being build. Does
     * nothing if the constant pool already contains a similar item. <i>This
     * method is intended for {@link Attribute} sub classes, and is normally not
     * needed by class generators or adapters.</i>
     */
    public int newUTF8(final String value) {
        key.set(UTF8, value, null, null);
        Item result = get(key);
        if (result == null) {
            pool.putByte(UTF8).putUTF8(value);
            result = new Item(index++, key);
            put(result);
        }
        return result.index;
    }

    /**
     * Adds a number or string constant to the constant pool of the class being
     * build. Does nothing if the constant pool already contains a similar item.
     */
    Item newConstItem(final Object cst) {

        if (cst instanceof Integer) {
            int val = ((Integer) cst).intValue();
            return newInteger(val);
        } else if (cst instanceof Byte) {
            int val = ((Byte) cst).intValue();
            return newInteger(val);
        } else if (cst instanceof Character) {
            int val = ((Character) cst).charValue();
            return newInteger(val);
        } else if (cst instanceof Short) {
            int val = ((Short) cst).intValue();
            return newInteger(val);
        } else if (cst instanceof Boolean) {
            int val = ((Boolean) cst).booleanValue() ? 1 : 0;
            return newInteger(val);
        } else if (cst instanceof Float) {
            float val = ((Float) cst).floatValue();
            return newFloat(val);
        } else if (cst instanceof Long) {
            long val = ((Long) cst).longValue();
            return newLong(val);
        } else if (cst instanceof Double) {
            double val = ((Double) cst).doubleValue();
            return newDouble(val);
        } else if (cst instanceof String) {
            return newString((String) cst);
        } else if (cst instanceof Type) {
            Type t = (Type) cst;
            return newClassItem(t.getSort() == Type.OBJECT ? t.getInternalName() : t.getDescriptor());
        } else {
            throw new IllegalArgumentException("value " + cst);
        }
    }

    /**
     * Adds a class reference to the constant pool of the class being build.
     * Does nothing if the constant pool already contains a similar item.
     *
     */
    Item newClassItem(final String value) {
        key2.set(CLASS, value, null, null);
        Item result = get(key2);
        if (result == null) {
            pool.put12(CLASS, newUTF8(value));
            result = new Item(index++, key2);
            put(result);
        }
        return result;
    }



    /**
     * Adds a number or string constant to the constant pool of the class being
     * build. Does nothing if the constant pool already contains a similar item.
     * <i>This method is intended for {@link Attribute} sub classes, and is
     * normally not needed by class generators or adapters.</i>
     */
    public int newConst(final Object cst) {
        return newConstItem(cst).index;
    }

    /**
     * Adds an integer to the constant pool of the class being build. Does
     * nothing if the constant pool already contains a similar item.
     */
    Item newInteger(final int value) {
        key.set(value);
        Item result = get(key);
        if (result == null) {
            pool.putByte(INT).putInt(value);
            result = new Item(index++, key);
            put(result);
        }
        return result;
    }

    /**
     * Adds a float to the constant pool of the class being build. Does nothing
     * if the constant pool already contains a similar item.
     */
    Item newFloat(final float value) {
        key.set(value);
        Item result = get(key);
        if (result == null) {
            pool.putByte(FLOAT).putInt(key.intVal);
            result = new Item(index++, key);
            put(result);
        }
        return result;
    }

    /**
     * Adds a long to the constant pool of the class being build. Does nothing
     * if the constant pool already contains a similar item.
     */
    Item newLong(final long value) {
        key.set(value);
        Item result = get(key);
        if (result == null) {
            pool.putByte(LONG).putLong(value);
            result = new Item(index, key);
            put(result);
            index += 2;
        }
        return result;
    }

    /**
     * Adds a double to the constant pool of the class being build. Does nothing
     * if the constant pool already contains a similar item.
     */
    Item newDouble(final double value) {
        key.set(value);
        Item result = get(key);
        if (result == null) {
            pool.putByte(DOUBLE).putLong(key.longVal);
            result = new Item(index, key);
            put(result);
            index += 2;
        }
        return result;
    }

    /**
     * Adds a string to the constant pool of the class being build. Does nothing
     * if the constant pool already contains a similar item.
     */
    private Item newString(final String value) {
        key2.set(STR, value, null, null);
        Item result = get(key2);
        if (result == null) {
            pool.put12(STR, newUTF8(value));
            result = new Item(index++, key2);
            put(result);
        }
        return result;
    }

    //*****************************************************
    // GET and PUT:
    //*****************************************************

    /**
     * Returns the constant pool's hash table item which is equal to the given
     * item.
     */
    private Item get(final Item key) {
        Item i = items[key.hashCode % items.length];
        while (i != null && !key.isEqualTo(i)) {
            i = i.next;
        }
        return i;
    }

    /**
     * Puts the given item in the constant pool's hash table. The hash table
     * <i>must</i> not already contains this item.
     */
    private void put(final Item i) {
        if (index > threshold) {
            int ll = items.length;
            int nl = ll * 2 + 1;
            Item[] newItems = new Item[nl];
            for (int l = ll - 1; l >= 0; --l) {
                Item j = items[l];
                while (j != null) {
                    int index = j.hashCode % newItems.length;
                    Item k = j.next;
                    j.next = newItems[index];
                    newItems[index] = j;
                    j = k;
                }
            }
            items = newItems;
            threshold = (int) (nl * 0.75);
        }
        int index = i.hashCode % items.length;
        i.next = items[index];
        items[index] = i;
    }

    /**
     * Puts one byte and two shorts into the constant pool.
     */
    private void put122(final int b, final int s1, final int s2) {
        pool.put12(b, s1).putShort(s2);
    }

    //*****************************************************
    // To Byte Array:
    //*****************************************************

    public byte[] toByteArray() {

        int size = 24 + (2 * interfaceCount);

        //***************************************
        // Calculate the field data size.
        //***************************************
        int nbFields = 0;
        FieldWriter fb = firstField;
        while (fb != null) {
            ++nbFields;
            size += fb.getSize();
            fb = fb.next;
        }

        //***************************************
        // Calculate method data size.
        //***************************************
        int nbMethods = 0;
        MethodWriter mb = firstMethod;
        while (mb != null) {
            ++nbMethods;
            size += mb.getSize();
            mb = mb.next;
        }
        int attributeCount = 0;

        size += pool.length;

        //***************************************
        // Write the class data.
        //***************************************
        ByteVector out = new ByteVector(size);
        out.putInt(0xCAFEBABE).putInt(version);
        out.putShort(index).putByteArray(pool.data, 0, pool.length);
        out.putShort(access).putShort(name).putShort(superName);
        out.putShort(interfaceCount);

        for (int i = 0; i < interfaceCount; ++i) {
            out.putShort(interfaces[i]);
        }

        //***************************************
        // Write the field data
        //***************************************
        out.putShort(nbFields);
        fb = firstField;
        while (fb != null) {
            fb.put(out);
            fb = fb.next;
        }
        //***************************************
        // Write the method data
        //***************************************
        out.putShort(nbMethods);
        mb = firstMethod;
        while (mb != null) {
            mb.put(out);
            mb = mb.next;
        }
        out.putShort(attributeCount);

        if (attrs != null) {
            attrs.put(this, null, 0, -1, -1, out);
        }

        return out.data;
    } // End of the Method toByteArray
}
