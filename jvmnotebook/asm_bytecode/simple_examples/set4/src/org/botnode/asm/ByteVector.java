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
 * A dynamically extensible vector of bytes. This class is roughly equivalent to
 * a DataOutputStream on top of a ByteArrayOutputStream, but is more efficient.
 *
 * @author Eric Bruneton
 */
public class ByteVector {

    /**
     * The content of this vector.
     */
    byte[] data;

    int length;

    public ByteVector() {
        data = new byte[64];
    }

    public ByteVector(final int initialSize) {
        data = new byte[initialSize];
    }

    public ByteVector putByte(final int b) {
        int length = this.length;
        if (length + 1 > data.length) {
            enlarge(1);
        }
        data[length++] = (byte) b;
        this.length = length;
        return this;
    }

    ByteVector put11(final int b1, final int b2) {
        int length = this.length;
        if (length + 2 > data.length) {
            enlarge(2);
        }
        byte[] data = this.data;
        data[length++] = (byte) b1;
        data[length++] = (byte) b2;
        this.length = length;
        return this;
    }

    public ByteVector putShort(final int s) {
        int length = this.length;
        if (length + 2 > data.length) {
            enlarge(2);
        }
        byte[] data = this.data;
        data[length++] = (byte) (s >>> 8);
        data[length++] = (byte) s;
        this.length = length;
        return this;
    }

    ByteVector put12(final int b, final int s) {
        int length = this.length;
        if (length + 3 > data.length) {
            enlarge(3);
        }
        byte[] data = this.data;
        data[length++] = (byte) b;
        data[length++] = (byte) (s >>> 8);
        data[length++] = (byte) s;
        this.length = length;
        return this;
    }

    public ByteVector putInt(final int i) {
        int length = this.length;
        if (length + 4 > data.length) {
            enlarge(4);
        }
        byte[] data = this.data;
        data[length++] = (byte) (i >>> 24);
        data[length++] = (byte) (i >>> 16);
        data[length++] = (byte) (i >>> 8);
        data[length++] = (byte) i;
        this.length = length;
        return this;
    }

    public ByteVector putLong(final long l) {
        int length = this.length;
        if (length + 8 > data.length) {
            enlarge(8);
        }
        byte[] data = this.data;
        int i = (int) (l >>> 32);
        data[length++] = (byte) (i >>> 24);
        data[length++] = (byte) (i >>> 16);
        data[length++] = (byte) (i >>> 8);
        data[length++] = (byte) i;
        i = (int) l;
        data[length++] = (byte) (i >>> 24);
        data[length++] = (byte) (i >>> 16);
        data[length++] = (byte) (i >>> 8);
        data[length++] = (byte) i;
        this.length = length;
        return this;
    }

    public ByteVector putUTF8(final String s) {
        int charLength = s.length();
        if (length + 2 + charLength > data.length) {
            enlarge(2 + charLength);
        }
        int len = length;
        byte[] data = this.data;

        data[len++] = (byte) (charLength >>> 8);
        data[len++] = (byte) charLength;
        for (int i = 0; i < charLength; ++i) {
            char c = s.charAt(i);
            if (c >= '\001' && c <= '\177') {
                data[len++] = (byte) c;
            } else {
                int byteLength = i;
                for (int j = i; j < charLength; ++j) {
                    c = s.charAt(j);
                    if (c >= '\001' && c <= '\177') {
                        byteLength++;
                    } else if (c > '\u07FF') {
                        byteLength += 3;
                    } else {
                        byteLength += 2;
                    }
                }
                data[length] = (byte) (byteLength >>> 8);
                data[length + 1] = (byte) byteLength;
                if (length + 2 + byteLength > data.length) {
                    length = len;
                    enlarge(2 + byteLength);
                    data = this.data;
                }
                for (int j = i; j < charLength; ++j) {
                    c = s.charAt(j);
                    if (c >= '\001' && c <= '\177') {
                        data[len++] = (byte) c;
                    } else if (c > '\u07FF') {
                        data[len++] = (byte) (0xE0 | c >> 12 & 0xF);
                        data[len++] = (byte) (0x80 | c >> 6 & 0x3F);
                        data[len++] = (byte) (0x80 | c & 0x3F);
                    } else {
                        data[len++] = (byte) (0xC0 | c >> 6 & 0x1F);
                        data[len++] = (byte) (0x80 | c & 0x3F);
                    }
                }
                break;
            }
        }
        length = len;
        return this;
    }

    public ByteVector putByteArray(final byte[] b, final int off, final int len) {
        if (length + len > data.length) {
            enlarge(len);
        }
        if (b != null) {
            System.arraycopy(b, off, data, length, len);
        }
        length += len;
        return this;
    }

    private void enlarge(final int size) {
        int length1 = 2 * data.length;
        int length2 = length + size;
        byte[] newData = new byte[length1 > length2 ? length1 : length2];
        System.arraycopy(data, 0, newData, 0, length);
        data = newData;
    }
}
