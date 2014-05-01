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
package com.raytheon.uf.common.util;

import java.io.IOException;
import java.util.Arrays;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Pools ByteArrayOutputStream objects. The lookup of a ByteArrayOutputStream
 * will always return without blocking. Will instead discard excessive objects
 * on return to the pool. Stream objects will also be discarded if they exceed
 * the maximum size. Relies on stream being closed to return the stream to the
 * pool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ByteArrayOutputStreamPool {
    private static final int MEGABYTE = 1024 * 1024;

    private static final ByteArrayOutputStreamPool instance = new ByteArrayOutputStreamPool();

    /** Pooled output streams for performance **/
    private ArrayBlockingQueue<ByteArrayOutputStream> streams;

    private int maxPoolSize = 8;

    private int initStreamSize = MEGABYTE;

    private int maxStreamSize = (int) (5.5 * MEGABYTE);

    public static ByteArrayOutputStreamPool getInstance() {
        return instance;
    }

    private ByteArrayOutputStreamPool() {
        streams = new ArrayBlockingQueue<ByteArrayOutputStream>(maxPoolSize);
    }

    /**
     * Returns a ByteArrayOutputStream whose initial capacity is at least the
     * initStreamSize. Close must be called on the returned object to return the
     * stream to the pool.
     * 
     * @param minInitialSize
     * @return
     */
    public ByteArrayOutputStream getStream() {
        return getStream(initStreamSize);
    }

    /**
     * Returns a ByteArrayOutputStream whose initial capacity is at least the
     * minInitialSize. Close must be called on the returned object to return the
     * stream to the pool.
     * 
     * @param minInitialSize
     * @return
     */
    public ByteArrayOutputStream getStream(int minInitialSize) {
        ByteArrayOutputStream rval = streams.poll();
        if (rval == null) {
            rval = new ByteArrayOutputStream(minInitialSize);
        } else if (rval.getCapacity() < minInitialSize) {
            rval.setCapacity(minInitialSize);
        }

        return rval;
    }

    public void setMaxPoolSize(int size) {
        if (size > 0) {
            ArrayBlockingQueue<ByteArrayOutputStream> tmp = new ArrayBlockingQueue<ByteArrayOutputStream>(
                    size);
            streams.drainTo(tmp, size);
            streams = tmp;
        } else {
            // throw illegal arg exception
            streams = new ArrayBlockingQueue<ByteArrayOutputStream>(1);
        }
    }

    /**
     * 
     * @param streamInitSize
     */
    public void setInitStreamSize(double initStreamSize) {
        this.initStreamSize = (int) (initStreamSize * MEGABYTE);
        ;
    }

    /**
     * 
     * @param streamMaxSize
     */
    public void setMaxStreamSize(double maxStreamSize) {
        this.maxStreamSize = (int) (maxStreamSize * MEGABYTE);
    }

    /**
     * Returns the stream to the pool if the stream is less than maxStreamSize
     * and if their is room in the pool.
     * 
     * @param stream
     */
    protected void returnToPool(ByteArrayOutputStream stream) {
        if (stream.getCapacity() <= maxStreamSize) {
            stream.reset();
            streams.offer(stream);
        }
    }

    /**
     * Override the base ByteArrayOutputStream to return to the pool on close.
     */
    public class ByteArrayOutputStream extends java.io.ByteArrayOutputStream {
        public ByteArrayOutputStream(int size) {
            super(size);
        }

        public int getCapacity() {
            return this.buf.length;
        }

        public void setCapacity(int length) {
            this.buf = Arrays.copyOf(this.buf, length);
            if (this.buf.length < this.count) {
                this.count = this.buf.length - 1;
            }

        }

        /**
         * This returns the underlying array of the output stream. Care should
         * be taken to keep count in sync if the underlying array is changed.
         * 
         * @return The underlying array.
         */
        public byte[] getUnderlyingArray() {
            return this.buf;
        }

        /**
         * Sets the current count. This should only be used if the
         * underlyingArray was changed outside of this classes knowledge.
         * 
         * @param count
         */
        public void setCount(int count) {
            if (count < this.buf.length) {
                this.count = count;
            } else {
                this.count = this.buf.length - 1;
            }

        }

        @Override
        public void close() throws IOException {
            ByteArrayOutputStreamPool.getInstance().returnToPool(this);
        }

    }
}
