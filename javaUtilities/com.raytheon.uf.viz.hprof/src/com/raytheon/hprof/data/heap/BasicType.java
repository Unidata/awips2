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
package com.raytheon.hprof.data.heap;

import java.nio.ByteBuffer;

import com.raytheon.hprof.BigByteBuffer;
import com.raytheon.hprof.Id;
import com.raytheon.hprof.data.HeapDumpRecord;

/**
 * 
 * Represents a basic type within a {@link HeapDumpRecord}. Used primarily for
 * fields of objects starts with a byte indicating the type of the field and
 * depending on the bytes holds either the primitive value or an object id.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 08, 2014  2648     bsteffen    Initial doc
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class BasicType {

    private static final byte OBJECT = 2;

    private static final byte BOOLEAN = 4;

    private static final byte CHAR = 5;

    private static final byte FLOAT = 6;

    private static final byte DOUBLE = 7;

    private static final byte BYTE = 8;

    private static final byte SHORT = 9;

    private static final byte INT = 10;

    private static final byte LONG = 11;

    private final byte type;

    private final byte[] data;

    public BasicType(BigByteBuffer buffer, int idSize) {
        this(buffer, buffer.get(), idSize);
    }

    public BasicType(BigByteBuffer buffer, byte type, int idSize) {
        this.type = type;
        switch (type) {
        case OBJECT:
            data = new byte[idSize];
            break;
        case FLOAT:
        case INT:
            data = new byte[4];
            break;
        case BOOLEAN:
        case BYTE:
            data = new byte[1];
            break;
        case CHAR:
        case SHORT:
            data = new byte[2];
            break;
        case DOUBLE:
        case LONG:
            data = new byte[8];
            break;
        default:
            throw new IllegalArgumentException("Unknown basic type "
                    + Integer.toHexString(type & 0xFF));
        }
        buffer.get(data);
    }

    public boolean isObject() {
        return type == OBJECT;
    }

    public boolean isBoolean() {
        return type == BOOLEAN;
    }

    public boolean isChar() {
        return type == CHAR;
    }

    public boolean isFloat() {
        return type == FLOAT;
    }

    public boolean isDouble() {
        return type == DOUBLE;
    }

    public boolean isByte() {
        return type == BYTE;
    }

    public boolean isShort() {
        return type == SHORT;
    }

    public boolean isInt() {
        return type == INT;
    }

    public boolean isLong() {
        return type == LONG;
    }

    public Id getObjectId() {
        if (!isObject()) {
            throw new IllegalStateException(type + " is not an OBJECT");
        }
        return new Id(ByteBuffer.wrap(data), data.length);
    }

    public boolean getBoolean() {
        if (!isBoolean()) {
            throw new IllegalStateException(type + " is not a BOOLEAN");
        }
        return ByteBuffer.wrap(data).get() != 0;
    }

    public char getChar() {
        if (!isChar()) {
            throw new IllegalStateException(type + " is not a CHAR");
        }
        return ByteBuffer.wrap(data).getChar();
    }

    public float getFloat() {
        if (!isFloat()) {
            throw new IllegalStateException(type + " is not a FLOAT");
        }
        return ByteBuffer.wrap(data).getFloat();
    }

    public double getDouble() {
        if (!isDouble()) {
            throw new IllegalStateException(type + " is not a DOUBLE");
        }
        return ByteBuffer.wrap(data).getDouble();
    }

    public byte getByte() {
        if (!isByte()) {
            throw new IllegalStateException(type + " is not a BYTE");
        }
        return data[0];
    }

    public short getShort() {
        if (!isShort()) {
            throw new IllegalStateException(type + " is not a SHORT");
        }
        return ByteBuffer.wrap(data).getShort();
    }

    public int getInt() {
        if (!isInt()) {
            throw new IllegalStateException(type + " is not an INT");
        }
        return ByteBuffer.wrap(data).getInt();
    }

    public long getLong() {
        if (!isLong()) {
            throw new IllegalStateException(type + " is not a LONG");
        }
        return ByteBuffer.wrap(data).getLong();
    }

    public int getSize() {
        return data.length;
    }

    @Override
    public String toString() {
        switch (type) {
        case OBJECT:
            return "Object: " + getObjectId();
        case FLOAT:
            return "Float: " + getFloat();
        case INT:
            return "Int: " + getInt();
        case BOOLEAN:
            return "Boolean: " + getBoolean();
        case BYTE:
            return "Byte: " + getByte();
        case CHAR:
            return "Char: " + getChar();
        case SHORT:
            return "Short: " + getShort();
        case DOUBLE:
            return " Double: " + getDouble();
        case LONG:
            return "Long: " + getLong();
        default:
            return "Unknown basic type " + Integer.toHexString(type & 0xFF);
        }
    }

}
