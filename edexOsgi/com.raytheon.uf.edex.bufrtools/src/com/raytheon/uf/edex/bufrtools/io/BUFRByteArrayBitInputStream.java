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
package com.raytheon.uf.edex.bufrtools.io;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRByteArrayBitInputStream extends BUFRBitInputStream {
//    private static final int[] BIT_MASK = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04,
//            0x02, 0x01, };
//
    private static final char WORD_MASK = 0xFFFF;
    
    private static final char[] BIT_MASK = {
        0x8000,0x4000,0x2000,0x1000,0x0800,0x0400,0x0200,0x0100,0x0080,0x0040,0x0020,0x0010,0x0008,0x0004,0x0002,0x0001};

    private static final char[] AVAIL_MASK = {
        0xFFFF,0x7FFF,0x3FFF,0x1FFF,0x0FFF,0x07FF,0x03FF,0x01FF,0x00FF,0x007F,0x003F,0x001F,0x000F,0x0007,0x0003,0x0001};
    
    private long numberBits;

    private char [] backingArray;

    private long currBitPosition = 0;

    private long bitsAvailable = 0;
    
    private long markBitPosition = 0;

    private boolean streamClosed = false;

    /**
     * 
     */
    public BUFRByteArrayBitInputStream(byte[] streamData) {
        if (streamData != null) {
            // Set the number of actual data bits present. Calculate from streamData, not
            // backingArray because of possible padding.
            numberBits = streamData.length * 8;
            bitsAvailable = numberBits;

            byte [] inData = null;
            if(streamData.length % 2 == 1) {
                inData = new byte [streamData.length + 1];
                System.arraycopy(streamData,0,inData,0,streamData.length);
                inData[streamData.length] = 0;
            } else {
                inData = streamData;
            }
            CharBuffer cf = ByteBuffer.wrap(inData).asCharBuffer();
            
            backingArray = new char[inData.length / 2];
            for(int i = 0;i < cf.limit();i++) {
                backingArray[i] = cf.get();
            }
        } else {
            backingArray = null;
            numberBits = 0;
        }
        currBitPosition = 0;
        markBitPosition = 0;
        streamClosed = false;
    }

    /**
     * Returns an estimate of the number of bits that can be read (or skipped
     * over) from this input stream without blocking by the next invocation of a
     * method for this input stream. Note that this implementation never blocks.
     */
    @Override
    public long available() {
        return bitsAvailable;
    }

    /**
     * Closes this stream and releases any system resources associated with it.
     * If the stream is already closed then invoking this method has no effect.
     * 
     * @throws IOException
     *             if an I/O error occurs
     */
    @Override
    public void close() throws IOException {
        // If the stream has already been closed, just return.
        if (!streamClosed) {
            backingArray = null;
            streamClosed = true;
            numberBits = 0;
            bitsAvailable = 0;
        }
    }

    /**
     * Marks the current position in this input stream.
     * 
     * @param readlimit
     *            Read limit is not used for this implementation. Any value can
     *            be used.
     */
    @Override
    public void mark(int readlimit) {
        if (!streamClosed) {
            markBitPosition = currBitPosition;
        }
    }

    /**
     * Tests if this input stream supports the mark and reset methods.
     * 
     * @return True. This stream support mark and reset.
     */
    @Override
    public boolean markSupported() {
        return true;
    }

    /**
     * Reads the next bit of data from the input stream.
     * 
     * @return The next bit of data from the input stream.
     */
    @Override
    public int read() {
        int bitData = -1;
        if (!streamClosed && (bitsAvailable > 0)) {

            int position = (int) currBitPosition / 16;
            int bitPos = (int) currBitPosition % 16;

            bitData = backingArray[position] & BIT_MASK[bitPos];
            if(bitData != 0) {
                bitData = 1;
            }
            currBitPosition++;
            bitsAvailable--;
        }
        return bitData;
    }

    /**
     * Reads a specified number of bits of data from the input stream.
     * 
     * @param numberOfBits
     *            The number of bits to be read.
     * @return The next bit of data from the input stream.
     */
    @Override
    public long read(int numberOfBits) {
        long bitData = 0;
        
        if (!streamClosed && (bitsAvailable > 0)) {

            int position = (int) currBitPosition / 16;
            int bitPos = (int) currBitPosition % 16;
            int bitsAvail = 16 - bitPos;
            if(numberOfBits < bitsAvail) {
                // We have enough data in the current word to fill the request,
                // so we can do the entire read here.
                bitData = backingArray[position] & AVAIL_MASK[bitPos];
                int shift = ((16 - bitPos) - numberOfBits);
                bitData >>= shift; //
                currBitPosition += numberOfBits;
                bitsAvailable -= numberOfBits;
            } else {
                // Need to split the request across 2 or more words.
                bitData = backingArray[position] & AVAIL_MASK[bitPos];
                numberOfBits -= bitsAvail;
                currBitPosition += bitsAvail;
                bitsAvailable -= bitsAvail;
                // make room for remaining bits
                bitData <<= numberOfBits; //
                bitData |= read(numberOfBits);
            }
        }        
        
        return bitData;
    }

    /**
     * Repositions this stream to the position at the time the mark method was
     * last called on this input stream.
     */
    @Override
    public void reset() {
        if (!streamClosed) {
            currBitPosition = markBitPosition;
            bitsAvailable = numberBits - currBitPosition;
        }
    }

    /**
     * 
     */
    public void rewind() {
        if (!streamClosed) {
            currBitPosition = 0;
            markBitPosition = 0;
            bitsAvailable = numberBits;
        }
    }

    /**
     * Skips over and discards n bits of data from this input stream.
     * 
     * @param skipAmount
     *            The number of bits to be skipped.
     * @return The number of bits that were skipped.
     */
    @Override
    public long skip(long skipAmount) {
        if (!streamClosed) {
            if ((skipAmount > 0) && (skipAmount <= bitsAvailable)) {
                currBitPosition += skipAmount;
                bitsAvailable -= skipAmount;
            }
        }
        return 0;
    }
    
    
    
    public static final void main(String [] args) {
        
        
        byte [] data = {
            (byte)0xAA,(byte)0xBB,(byte)0xBC,(byte)0x67,(byte)0xAF,(byte)0xE1,(byte)0xD3,};

        long n = 0;
        
        
        BUFRBitInputStream bis = new BUFRByteArrayBitInputStream(data);

        long total = 0;
        long start = System.currentTimeMillis();
        for(int i = 0;i < 100000;i++) {
            bis.mark(0);
            
            n = bis.read(3);
            total += n;
            // System.out.println(String.format("[%4X]",n));

            n = bis.read(1);
            total += n;
            // System.out.println(String.format("[%4X]",n));

            n = bis.read(10);
            total += n;
            // System.out.println(String.format("[%4X]",n));

            n = bis.read(4);
            total += n;
            // System.out.println(String.format("[%4X]",n));
            
            n = bis.read(14);
            total += n;
            // System.out.println(String.format("[%4X]",n));

            bis.reset();
            
            n = bis.read(4);
            total += n;
            // System.out.println(String.format("[%4X]",n));
            
            n = bis.read(15);
            total += n;
            // System.out.println(String.format("[%4X]",n));
            
        }
        long stop = System.currentTimeMillis();
        System.out.println(String.format("Elapsed time = %d",(stop - start)));

    }
    
}
