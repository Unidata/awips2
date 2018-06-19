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
package com.raytheon.edex.plugin.binlightning.total;

import java.nio.ByteBuffer;

import com.raytheon.uf.common.numeric.UnsignedNumbers;

/**
 * ByteBuffer wrapper that keeps track of checksums
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 03, 2014  3226      bclement    Initial creation
 * Jun 09, 2014 3226       bclement    Added ByteBuffer constructor
 * Jul 01, 2015 4581       skorolev    Added condition in the getSum(int)
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ChecksumByteBuffer {

    private static final int SHORT_SIZE = Short.SIZE / Byte.SIZE;

    private static final int INT_SIZE = Integer.SIZE / Byte.SIZE;

    private static final int LONG_SIZE = Integer.SIZE / Byte.SIZE;

    private final ByteBuffer buff;

    private long totalSum;

    private long packetSum;

    /**
     * @see ByteBuffer#wrap(byte[])
     * @param data
     */
    public ChecksumByteBuffer(byte[] data) {
        this.buff = ByteBuffer.wrap(data);
    }

    /**
     * @param buff
     */
    public ChecksumByteBuffer(ByteBuffer buff) {
        this.buff = buff;
    }

    /**
     * get the sum of the next numberOfBytes worth of data
     * 
     * @param numberOfBytes
     * @return
     */
    private long getSum(int numberOfBytes) {
        int start = buff.position();
        int end = start + numberOfBytes;
        long rval = 0;
        if (buff.remaining() < numberOfBytes) {
            throw new IllegalArgumentException("Unable to get checksum for "
                    + numberOfBytes + " bytes, only " + buff.remaining()
                    + " bytes in buffer.");
        }
        for (int i = start; i < end; ++i) {
            rval += UnsignedNumbers.ubyteToShort(buff.get(i));
        }
        return rval;
    }

    /**
     * @see ByteBuffer#get()
     * @return
     */
    public byte get() {
        byte rval = buff.get();
        short unsignedRval = UnsignedNumbers.ubyteToShort(rval);
        totalSum += unsignedRval;
        packetSum += unsignedRval;
        return rval;
    }

    /**
     * @see ByteBuffer#getShort()
     * @return
     */
    public short getShort() {
        long sum = getSum(SHORT_SIZE);
        totalSum += sum;
        packetSum += sum;
        return buff.getShort();
    }

    /**
     * @see ByteBuffer#getInt()
     * @return
     */
    public int getInt() {
        long sum = getSum(INT_SIZE);
        totalSum += sum;
        packetSum += sum;
        return buff.getInt();
    }

    /**
     * @see ByteBuffer#getLong()
     * @return
     */
    public long getLong() {
        long sum = getSum(LONG_SIZE);
        totalSum += sum;
        packetSum += sum;
        return buff.getLong();
    }

    /**
     * reset the current packet sum to zero
     */
    public void resetPacketSum() {
        packetSum = 0;
    }

    /**
     * reset all sums to zero
     */
    public void resetAllSums() {
        resetPacketSum();
        totalSum = 0;
    }

    /**
     * @return the totalSum
     */
    public long getTotalSum() {
        return totalSum;
    }

    /**
     * @return the packetSum
     */
    public long getPacketSum() {
        return packetSum;
    }

    /**
     * @see ByteBuffer#limit()
     * @return
     */
    public int size() {
        return buff.limit();
    }

    /**
     * @see ByteBuffer#position()
     * @return
     */
    public int position() {
        return buff.position();
    }

}
