package com.raytheon.uf.common.serialization.thrift;

import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TStruct;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TTransport;

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

/**
 * Implement serialization using a self-describing version of the Thrift binary
 * protocol. <BR>
 * <BR>
 * <B>Differences from standard thrift:</B>
 * <UL>
 * <LI>Structs have their types (class names) encoded in the header
 * <LI>Fields have their names encoded in the header
 * <LI>float types are supported
 * </UL>
 * 
 * 
 * <BR>
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 7, 2008				chammack	Initial creation
 * Jun 17, 2010   #5091     njensen     Added primitive list methods
 * Jun 12, 2013    2102     njensen     Added max read length to prevent out
 *                                       of memory errors due to bad stream
 * Jul 23, 2013    2215     njensen     Updated for thrift 0.9.0
 * Aug 06, 2013    2228     njensen     Overrode readBinary() to ensure it
 *                                       doesn't read too much
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SelfDescribingBinaryProtocol extends TBinaryProtocol {

    public static final byte FLOAT = 64;

    /**
     * This is to ensure a safety check because if the stream has bad bytes at
     * the start, thrift may try to allocate something huge, such as GBs of
     * data, and then the JVM will blow up about OutOfMemory errors.
     **/
    private static int MAX_READ_LENGTH;

    static {
        try {
            int sizeInMB = Integer.parseInt(System
                    .getProperty("thrift.stream.maxsize"));
            MAX_READ_LENGTH = sizeInMB * 1024 * 1024;
        } catch (Throwable t) {
            System.err
                    .println("Error reading property thrift.stream.maxsize - falling back to default of 200 MB");
            t.printStackTrace();
            MAX_READ_LENGTH = 200 * 1024 * 1024;
        }
    }

    public SelfDescribingBinaryProtocol(TTransport trans) {
        this(trans, false, true);
    }

    public SelfDescribingBinaryProtocol(TTransport trans, boolean strictRead,
            boolean strictWrite) {
        super(trans, strictRead, strictWrite);
        this.setReadLength(MAX_READ_LENGTH);
    }

    @Override
    public ByteBuffer readBinary() throws TException {
        int size = readI32();
        checkReadLength(size);
        byte[] buf = new byte[size];
        trans_.readAll(buf, 0, size);
        return ByteBuffer.wrap(buf);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.facebook.thrift.protocol.TBinaryProtocol#readFieldBegin()
     */
    @Override
    public TField readFieldBegin() throws TException {
        // This method was overriden to make the structs more self describing
        Byte type = readByte();
        String name = "";
        short id = (short) 0;
        if (type != TType.STOP) {
            name = readString();
            id = readI16();
        }
        return new TField(name, type, id);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.facebook.thrift.protocol.TBinaryProtocol#writeFieldBegin(com.facebook
     * .thrift.protocol.TField)
     */
    @Override
    public void writeFieldBegin(TField field) throws TException {
        // This method was overriden to make the structs more self describing
        writeByte(field.type);
        writeString(field.name);
        writeI16(field.id);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.facebook.thrift.protocol.TBinaryProtocol#readStructBegin()
     */
    @Override
    public TStruct readStructBegin() {
        // This method was overriden to make the structs more self describing
        String name;
        try {
            name = readString();
        } catch (TException e) {
            // TODO: unfortunately incompatible signatures prevent this from
            // being thrown up as a TException
            throw new RuntimeException(e);
        }
        return new TStruct(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.facebook.thrift.protocol.TBinaryProtocol#writeStructBegin(com.facebook
     * .thrift.protocol.TStruct)
     */
    @Override
    public void writeStructBegin(TStruct struct) {
        // This method was overriden to make the structs more self describing
        try {
            writeString(struct.name);
        } catch (TException e) {
            // TODO: unfortunately incompatible signatures prevent this from
            // being thrown up as a TException
            throw new RuntimeException(e);
        }
    }

    /**
     * Write a float
     * 
     * @param flt
     * @throws TException
     */
    public void writeFloat(float flt) throws TException {
        writeI32(Float.floatToIntBits(flt));
    }

    /**
     * Read a float
     * 
     * @return float
     * @throws TException
     */
    public float readFloat() throws TException {
        return Float.intBitsToFloat(readI32());
    }

    /**
     * Reads a set number of bytes into a buffer
     * 
     * @param length
     * @return
     * @throws TException
     */
    private ByteBuffer readBytes(int length) throws TException {
        byte[] b = new byte[length];
        int n = this.trans_.readAll(b, 0, length);
        if (n != length) {
            throw new TException("Bytes read does not match indicated size");
        }
        ByteBuffer buf = ByteBuffer.wrap(b);
        return buf;
    }

    /**
     * Read a list of floats
     * 
     * @param sz
     * @return data as floats
     * @throws TException
     */
    public float[] readF32List(int sz) throws TException {
        ByteBuffer buf = readBytes(sz * 4);
        FloatBuffer fbuf = buf.asFloatBuffer();
        float[] f = new float[sz];
        fbuf.get(f);
        return f;
    }

    /**
     * Write a list of floats
     * 
     * @param arr
     * @throws TException
     */
    public void writeF32List(float[] arr) throws TException {
        byte[] b = new byte[4 * arr.length];
        ByteBuffer bb = ByteBuffer.wrap(b);
        FloatBuffer fb = bb.asFloatBuffer();
        fb.put(arr);
        this.trans_.write(bb.array());
    }

    /**
     * Read a list of ints
     * 
     * @param sz
     * @return data as ints
     * @throws TException
     */
    public int[] readI32List(int sz) throws TException {
        ByteBuffer buf = readBytes(sz * 4);
        IntBuffer ibuf = buf.asIntBuffer();
        int[] i = new int[sz];
        ibuf.get(i);
        return i;
    }

    /**
     * Write a list of ints
     * 
     * @param arr
     * @throws TException
     */
    public void writeI32List(int[] arr) throws TException {
        byte[] b = new byte[4 * arr.length];
        ByteBuffer bb = ByteBuffer.wrap(b);
        IntBuffer ib = bb.asIntBuffer();
        ib.put(arr);
        this.trans_.write(bb.array());
    }

    /**
     * Read a list of doubles
     * 
     * @param sz
     * @return data as doubles
     * @throws TException
     */
    public double[] readD64List(int sz) throws TException {
        ByteBuffer buf = readBytes(sz * 8);
        DoubleBuffer pbuf = buf.asDoubleBuffer();
        double[] arr = new double[sz];
        pbuf.get(arr);
        return arr;
    }

    /**
     * Write a list of doubles
     * 
     * @param arr
     * @throws TException
     */
    public void writeD64List(double[] arr) throws TException {
        byte[] b = new byte[8 * arr.length];
        ByteBuffer bb = ByteBuffer.wrap(b);
        DoubleBuffer pb = bb.asDoubleBuffer();
        pb.put(arr);
        this.trans_.write(bb.array());
    }

    /**
     * Read a list of longs
     * 
     * @param sz
     * @return data as longs
     * @throws TException
     */
    public long[] readI64List(int sz) throws TException {
        ByteBuffer buf = readBytes(sz * 8);
        LongBuffer pbuf = buf.asLongBuffer();
        long[] arr = new long[sz];
        pbuf.get(arr);
        return arr;
    }

    /**
     * Write a list of longs
     * 
     * @param arr
     * @throws TException
     */
    public void writeI64List(long[] arr) throws TException {
        byte[] b = new byte[8 * arr.length];
        ByteBuffer bb = ByteBuffer.wrap(b);
        LongBuffer pb = bb.asLongBuffer();
        pb.put(arr);
        this.trans_.write(bb.array());
    }

    /**
     * Read a list of shorts
     * 
     * @param sz
     * @return data as shorts
     * @throws TException
     */
    public short[] readI16List(int sz) throws TException {
        ByteBuffer buf = readBytes(sz * 2);
        ShortBuffer pbuf = buf.asShortBuffer();
        short[] arr = new short[sz];
        pbuf.get(arr);
        return arr;
    }

    /**
     * Write a list of doubles
     * 
     * @param arr
     * @throws TException
     */
    public void writeI16List(short[] arr) throws TException {
        byte[] b = new byte[2 * arr.length];
        ByteBuffer bb = ByteBuffer.wrap(b);
        ShortBuffer pb = bb.asShortBuffer();
        pb.put(arr);
        this.trans_.write(bb.array());
    }

    /**
     * Read a list of bytes
     * 
     * @param sz
     * @return data as bytes
     * @throws TException
     */
    public byte[] readI8List(int sz) throws TException {
        ByteBuffer buf = readBytes(sz);
        return buf.array();
    }

    /**
     * Write a list of bytes
     * 
     * @param arr
     * @throws TException
     */
    public void writeI8List(byte[] arr) throws TException {
        this.trans_.write(arr);
    }

}
