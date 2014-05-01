/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.core.gl;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;

/**
 * GrowableBuffer
 * 
 * Represents a java NIO buffer that automatically resizes as needed (similar to
 * java Lists, but more efficient)
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 12, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public final class GrowableBuffer<B extends java.nio.Buffer> {
    // Don't grow more than this rate
    private static final int DIRECT_BUFFER_MAX_GROWTH_UNITS = 50000;

    private ByteBuffer directBuffer;

    private B backingBuffer;

    private boolean isDirect;

    private enum Type {
        BYTE, FLOAT, INTEGER, SHORT, LONG
    };

    private Type type;

    private int curPos;

    private int curSize;

    public GrowableBuffer(B backingBuffer) {

        isDirect = backingBuffer.isDirect();
        if (backingBuffer instanceof ByteBuffer) {
            type = Type.BYTE;
        } else if (backingBuffer instanceof FloatBuffer) {
            type = Type.FLOAT;
        } else if (backingBuffer instanceof IntBuffer) {
            type = Type.INTEGER;
        } else if (backingBuffer instanceof ShortBuffer) {
            type = Type.SHORT;
        } else if (backingBuffer instanceof LongBuffer) {
            type = Type.LONG;
        } else
            throw new IllegalArgumentException("Unsupported type "
                    + backingBuffer.getClass().getName());

        this.backingBuffer = backingBuffer;
        this.curPos = 0;
        this.curSize = this.backingBuffer.capacity();

    }

    public B getBuffer() {
        return this.backingBuffer;
    }

    public void put(int integer) {
        if (type != Type.INTEGER) {
            throw new UnsupportedOperationException(
                    "Unable to put integer into non-integer buffer");
        }

        ensureCapacity(1);
        backingBuffer.position(curPos);
        ((IntBuffer) backingBuffer).put(integer);
        curPos++;
    }

    public void put(short shortInteger) {
        if (type != Type.SHORT) {
            throw new UnsupportedOperationException(
                    "Unable to put short into non-short buffer");
        }

        ensureCapacity(1);
        backingBuffer.position(curPos);
        ((ShortBuffer) backingBuffer).put(shortInteger);
        curPos++;
    }

    public void put(float floatNumber) {
        if (type != Type.FLOAT) {
            throw new UnsupportedOperationException(
                    "Unable to put float into non-float buffer");
        }

        ensureCapacity(1);
        backingBuffer.position(curPos);
        ((FloatBuffer) backingBuffer).put(floatNumber);
        curPos++;
    }

    public void put(long longNumber) {
        if (type != Type.LONG) {
            throw new UnsupportedOperationException(
                    "Unable to put long into non-long buffer");
        }

        ensureCapacity(1);
        backingBuffer.position(curPos);
        ((LongBuffer) backingBuffer).put(longNumber);
        curPos++;
    }

    public void put(byte byteNumber) {
        if (type != Type.BYTE) {
            throw new UnsupportedOperationException(
                    "Unable to put byte into non-byte buffer");
        }

        ensureCapacity(1);
        backingBuffer.position(curPos);
        ((ByteBuffer) backingBuffer).put(byteNumber);
        curPos++;
    }

    public void put(byte[] byteArr) {
        if (type != Type.BYTE) {
            throw new UnsupportedOperationException(
                    "Unable to put byte[] into non-byte buffer");
        }

        ensureCapacity(byteArr.length);
        backingBuffer.position(curPos);
        ((ByteBuffer) backingBuffer).put(byteArr);
        curPos += byteArr.length;
        ;
    }

    public void put(float[] floatArr) {
        if (type != Type.BYTE) {
            throw new UnsupportedOperationException(
                    "Unable to put float[] into non-float buffer");
        }

        ensureCapacity(floatArr.length);
        backingBuffer.position(curPos);
        ((FloatBuffer) backingBuffer).put(floatArr);
        curPos += floatArr.length;
        ;
    }

    public void put(int[] integer) {
        if (type != Type.INTEGER) {
            throw new UnsupportedOperationException(
                    "Unable to put integer into non-integer buffer");
        }

        ensureCapacity(integer.length);
        backingBuffer.position(curPos);
        ((IntBuffer) backingBuffer).put(integer);
        curPos += integer.length;
        ;
    }

    @SuppressWarnings("unchecked")
    public void ensureCapacity(int numUnitsToIncrease) {
        if (curSize >= (curPos + numUnitsToIncrease)) {
            return; // No grow required
        }

        int factorIncrease = (int) (Math.min(DIRECT_BUFFER_MAX_GROWTH_UNITS,
                curSize * 0.5)) + curSize;
        int accomodateIncrease = (curPos + (numUnitsToIncrease));

        int sz = Math.max(factorIncrease, accomodateIncrease);

        int oldSize = curSize;
        int oldPos = curPos;
        curSize = sz;

        B oldBuffer = this.backingBuffer;
        ByteBuffer oldDirectBuffer = directBuffer;

        if (isDirect) {

            int szOfElement = 1;
            if (type == Type.FLOAT || type == Type.INTEGER) {
                szOfElement = 4;
            } else if (type == Type.SHORT) {
                szOfElement = 2;
            } else if (type == Type.LONG) {
                szOfElement = 8;
            }

            directBuffer = ByteBuffer.allocateDirect(szOfElement * sz);
            directBuffer.order(ByteOrder.nativeOrder());
            directBuffer.position(0);

            if (type == Type.FLOAT) {
                backingBuffer = (B) directBuffer.asFloatBuffer();
            } else if (type == Type.INTEGER) {
                backingBuffer = (B) directBuffer.asIntBuffer();
            } else if (type == Type.SHORT) {
                backingBuffer = (B) directBuffer.asShortBuffer();
            } else if (type == Type.LONG) {
                backingBuffer = (B) directBuffer.asLongBuffer();
            } else if (type == Type.BYTE) {
                backingBuffer = (B) directBuffer;
            }
        } else {
            if (type == Type.FLOAT) {
                backingBuffer = (B) FloatBuffer.allocate(sz);
            } else if (type == Type.INTEGER) {
                backingBuffer = (B) IntBuffer.allocate(sz);
            } else if (type == Type.SHORT) {
                backingBuffer = (B) ShortBuffer.allocate(sz);
            } else if (type == Type.LONG) {
                backingBuffer = (B) LongBuffer.allocate(sz);
            } else if (type == Type.BYTE) {
                backingBuffer = (B) ByteBuffer.allocate(sz);
            }
        }

        oldBuffer.limit(oldPos);
        oldBuffer.position(0);
        backingBuffer.position(0);

        if (type == Type.FLOAT) {
            ((FloatBuffer) backingBuffer).put((FloatBuffer) oldBuffer);
        } else if (type == Type.INTEGER) {
            ((IntBuffer) backingBuffer).put((IntBuffer) oldBuffer);
        } else if (type == Type.SHORT) {
            ((ShortBuffer) backingBuffer).put((ShortBuffer) oldBuffer);
        } else if (type == Type.LONG) {
            ((LongBuffer) backingBuffer).put((LongBuffer) oldBuffer);
        } else if (type == Type.BYTE) {
            ((ByteBuffer) backingBuffer).put((ByteBuffer) oldBuffer);
        }

        oldBuffer.limit(0);
        oldBuffer = null;

        if (oldDirectBuffer != null) {
            oldDirectBuffer.limit(0);
            oldDirectBuffer = null;
        }

        System.out.println("Grew from " + oldSize + " to " + curSize);
    }

    public void position(int position) {
        this.curPos = position;
        this.backingBuffer.position(position);
    }

    public void dispose() {
        if (backingBuffer != null) {
            this.backingBuffer.limit(0);
            this.backingBuffer = null;
        }

        this.directBuffer = null;
    }

    public void clear() {
        curPos = 0;
        this.backingBuffer.clear();
    }

    public void allocate(int size) {
        ensureCapacity(size);
    }
}
