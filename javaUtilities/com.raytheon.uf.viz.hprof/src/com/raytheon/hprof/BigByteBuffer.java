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
package com.raytheon.hprof;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;

/**
 * 
 * ByteBuffers cannot be used to index more than 2GB of data. Since some hprof
 * files are larger than that a BigByteBuffer is used to allow long indexing
 * into a much larger space. BigByteBuffers internally are represented by direct
 * memory mapped byte buffers that map the contents of a single file.
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
public class BigByteBuffer {

    private final long chunkSize = 1024 * 1024 * 256;

    private final ByteBuffer[] buffers;

    private final long offset;
    
    private final long capacity;
    
    private long mark = -1;

    private long position = 0;

    private long limit;

    public BigByteBuffer(String fileName) throws IOException {
        FileInputStream fis = new FileInputStream(fileName);
        FileChannel fileChannel = fis.getChannel();
        offset = 0;
        limit = capacity = fileChannel.size();
        int nBuffers = (int) (capacity / chunkSize);
        if (nBuffers * chunkSize < capacity) {
            nBuffers += 1;
        }
        buffers = new ByteBuffer[nBuffers];
        for (int i = 0; i < buffers.length; i += 1) {
            buffers[i] = fileChannel.map(MapMode.READ_ONLY, i * chunkSize,
                    Math.min(capacity - i * chunkSize, chunkSize));
        }
        fis.close();
    }

    protected BigByteBuffer(ByteBuffer[] buffers, long offset, long capacity) {
        this.buffers = buffers;
        this.offset = offset;
        this.capacity = capacity;
        this.limit = capacity;
    }

    public final long capacity() {
        return capacity;
    }

    public final long position() {
        return position;
    }

    public final BigByteBuffer position(long newPosition) {
        if ((newPosition > limit) || (newPosition < 0))
            throw new IllegalArgumentException();
        position = newPosition;
        if (mark > position)
            mark = -1;
        return this;
    }

    public final long limit() {
        return limit;
    }

    public final BigByteBuffer limit(long newLimit) {
        if ((newLimit > capacity) || (newLimit < 0))
            throw new IllegalArgumentException();
        limit = newLimit;
        if (position > limit)
            position = limit;
        if (mark > limit)
            mark = -1;
        return this;
    }

    public final BigByteBuffer rewind() {
        position = 0;
        mark = -1;
        return this;
    }

    public final long remaining() {
        return limit - position;
    }

    public final boolean hasRemaining() {
        return position < limit;
    }

    public BigByteBuffer slice() {
        return new BigByteBuffer(buffers, this.position() + offset,
                this.remaining());
    }

    public byte get() {
        return get(nextGetIndex());
    }

    public byte get(long index) {
        checkIndex(index);
        return getBuffer(index).get(bufferIndex(index));
    }

    public void get(byte[] dst) {
        long start = nextGetIndex(dst.length);
        ByteBuffer buffer = getBuffer(start);
        buffer.position(bufferIndex(start));
        int offset = 0;
        int remaining = dst.length;
        while (remaining > 0) {
            int length = Math.min(remaining, buffer.remaining());
            buffer.get(dst, offset, length);
            offset += length;
            remaining -= length;
            buffer = getBuffer(start + offset);
            buffer.position(bufferIndex(start + offset));

        }
    }

    public short getShort() {
        byte[] dst = new byte[2];
        get(dst);
        return ByteBuffer.wrap(dst).getShort();
    }

    public int getInt() {
        byte[] dst = new byte[4];
        get(dst);
        return ByteBuffer.wrap(dst).getInt();
    }

    public long getLong() {
        byte[] dst = new byte[8];
        get(dst);
        return ByteBuffer.wrap(dst).getLong();
    }

    final ByteBuffer getBuffer(long index) {
        return buffers[(int) ((offset + index) / chunkSize)];
    }

    final int bufferIndex(long index) {
        return (int) ((offset + index) % chunkSize);
    }

    final long nextGetIndex() {
        if (position >= limit)
            throw new BufferUnderflowException();
        return position++;
    }

    final long nextGetIndex(int nb) {
        if (limit - position < nb)
            throw new BufferUnderflowException();
        long p = position;
        position += nb;
        return p;
    }

    final long checkIndex(long i) {
        if ((i < 0) || (i >= limit))
            throw new IndexOutOfBoundsException();
        return i;
    }
}
