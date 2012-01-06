package org.apache.qpid.util.concurrent;
/*
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 */


import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Synchronous/Asynchronous puts. Asynchronous is easiest, just wait till can write to queue and deposit data.
 * Synchronous is harder. Deposit data, but then must wait until deposited element/elements are taken before being
 * allowed to unblock and continue. Consumer needs some options here too. Can just get the data from the buffer and
 * allow any producers unblocked as a result to continue, or can get data but continue blocking while the data is
 * processed before sending a message to do the unblocking. Synch/Asynch mode to be controlled by a switch.
 * Unblocking/not unblocking during consumer processing to be controlled by the consumers calls.
 *
 * <p/>Implementing sub-classes only need to supply an implementation of a queue to produce a valid concrete
 * implementation of this. This queue is only accessed through the methods {@link #insert}, {@link #extract},
 * {@link #getBufferCapacity()}, {@link #peekAtBufferHead()}. An implementation can override these methods to implement
 * the buffer other than by a queue, for example, by using an array.
 *
 * <p/>Normal queue methods to work asynchronously.
 * <p/>Put, take and drain methods from the BlockingQueue interface work synchronously but unblock producers immediately
 * when their data is taken.
 * <p/>The additional put, take and drain methods from the BatchSynchQueue interface work synchronously and provide the
 * option to keep producers blocked until the consumer decides to release them.
 *
 * <p/>Removed take method that keeps producers blocked as it is pointless. Essentially it reduces this class to
 * synchronous processing of individual data items, which negates the point of the hand-off design. The efficiency
 * gain of the hand off design comes in being able to batch consume requests, ammortizing latency (such as caused by io)
 * accross many producers. The only advantage of the single blocking take method is that it did take advantage of the
 * queue ordering, which ma be usefull, for example to apply a priority ordering amongst producers. This is also an
 * advantage over the java.util.concurrent.SynchronousQueue which doesn't have a backing queue which can be used to
 * apply orderings. If a single item take is really needed can just use the drainTo method with a maximum of one item.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * </table>
 */
public abstract class BatchSynchQueueBase<E> extends AbstractQueue<E> implements BatchSynchQueue<E>
{
    /** Used for logging. */
    private static final Logger log = LoggerFactory.getLogger(BatchSynchQueueBase.class);

    /** Holds a reference to the queue implementation that holds the buffer. */
    Queue<SynchRecordImpl<E>> buffer;

    /** Holds the number of items in the queue */
    private int count;

    /** Main lock guarding all access */
    private ReentrantLock lock;

    /** Condition for waiting takes */
    private Condition notEmpty;

    /** Condition for waiting puts */
    private Condition notFull;

    /**
     * Creates a batch synch queue without fair thread scheduling.
     */
    public BatchSynchQueueBase()
    {
        this(false);
    }

    /**
     * Ensures that the underlying buffer implementation is created.
     *
     * @param fair <tt>true</tt> if fairness is to be applied to threads waiting to access the buffer.
     */
    public BatchSynchQueueBase(boolean fair)
    {
        buffer = this.createQueue();

        // Create the buffer lock with the fairness flag set accordingly.
        lock = new ReentrantLock(fair);

        // Create the non-empty and non-full condition monitors on the buffer lock.
        notEmpty = lock.newCondition();
        notFull = lock.newCondition();
    }

    /**
     * Returns an iterator over the elements contained in this collection.
     *
     * @return An iterator over the elements contained in this collection.
     */
    public Iterator<E> iterator()
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Returns the number of elements in this collection.  If the collection contains more than
     * <tt>Integer.MAX_VALUE</tt> elements, returns <tt>Integer.MAX_VALUE</tt>.
     *
     * @return The number of elements in this collection.
     */
    public int size()
    {
        final ReentrantLock lock = this.lock;
        lock.lock();

        try
        {
            return count;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Inserts the specified element into this queue, if possible. When using queues that may impose insertion
     * restrictions (for example capacity bounds), method <tt>offer</tt> is generally preferable to method
     * {@link java.util.Collection#add}, which can fail to insert an element only by throwing an exception.
     *
     * @param e The element to insert.
     *
     * @return <tt>true</tt> if it was possible to add the element to this queue, else <tt>false</tt>
     */
    public boolean offer(E e)
    {
        if (e == null)
        {
            throw new NullPointerException();
        }

        final ReentrantLock lock = this.lock;
        lock.lock();

        try
        {
            return insert(e, false);
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Inserts the specified element into this queue, waiting if necessary up to the specified wait time for space to
     * become available.
     *
     * @param e       The element to add.
     * @param timeout How long to wait before giving up, in units of <tt>unit</tt>
     * @param unit    A <tt>TimeUnit</tt> determining how to interpret the <tt>timeout</tt> parameter.
     *
     * @return <tt>true</tt> if successful, or <tt>false</tt> if the specified waiting time elapses before space is
     *         available.
     *
     * @throws InterruptedException If interrupted while waiting.
     * @throws NullPointerException If the specified element is <tt>null</tt>.
     */
    public boolean offer(E e, long timeout, TimeUnit unit) throws InterruptedException
    {
        if (e == null)
        {
            throw new NullPointerException();
        }

        final ReentrantLock lock = this.lock;
        lock.lockInterruptibly();

        long nanos = unit.toNanos(timeout);

        try
        {
            do
            {
                if (insert(e, false))
                {
                    return true;
                }

                try
                {
                    nanos = notFull.awaitNanos(nanos);
                }
                catch (InterruptedException ie)
                {
                    notFull.signal(); // propagate to non-interrupted thread
                    throw ie;
                }
            }
            while (nanos > 0);

            return false;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Retrieves and removes the head of this queue, or <tt>null</tt> if this queue is empty.
     *
     * @return The head of this queue, or <tt>null</tt> if this queue is empty.
     */
    public E poll()
    {
        final ReentrantLock lock = this.lock;

        lock.lock();
        try
        {
            if (count == 0)
            {
                return null;
            }

            E x = extract(true, true).getElement();

            return x;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Retrieves and removes the head of this queue, waiting if necessary up to the specified wait time if no elements
     * are present on this queue.
     *
     * @param timeout How long to wait before giving up, in units of <tt>unit</tt>.
     * @param unit    A <tt>TimeUnit</tt> determining how to interpret the <tt>timeout</tt> parameter.
     *
     * @return The head of this queue, or <tt>null</tt> if the specified waiting time elapses before an element is present.
     *
     * @throws InterruptedException If interrupted while waiting.
     */
    public E poll(long timeout, TimeUnit unit) throws InterruptedException
    {
        final ReentrantLock lock = this.lock;
        lock.lockInterruptibly();
        try
        {
            long nanos = unit.toNanos(timeout);

            do
            {
                if (count != 0)
                {
                    E x = extract(true, true).getElement();

                    return x;
                }

                try
                {
                    nanos = notEmpty.awaitNanos(nanos);
                }
                catch (InterruptedException ie)
                {
                    notEmpty.signal(); // propagate to non-interrupted thread
                    throw ie;
                }
            }
            while (nanos > 0);

            return null;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Retrieves, but does not remove, the head of this queue, returning <tt>null</tt> if this queue is empty.
     *
     * @return The head of this queue, or <tt>null</tt> if this queue is empty.
     */
    public E peek()
    {
        final ReentrantLock lock = this.lock;
        lock.lock();

        try
        {
            return peekAtBufferHead();
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Returns the number of elements that this queue can ideally (in the absence of memory or resource constraints)
     * accept without blocking, or <tt>Integer.MAX_VALUE</tt> if there is no intrinsic limit.
     *
     * <p>Note that you <em>cannot</em> always tell if an attempt to <tt>add</tt> an element will succeed by
     * inspecting <tt>remainingCapacity</tt> because it may be the case that another thread is about to <tt>put</tt>
     * or <tt>take</tt> an element.
     *
     * @return The remaining capacity.
     */
    public int remainingCapacity()
    {
        final ReentrantLock lock = this.lock;
        lock.lock();

        try
        {
            return getBufferCapacity() - count;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Adds the specified element to this queue, waiting if necessary for space to become available.
     *
     * <p/>This method delegated to {@link #tryPut} which can raise {@link SynchException}s. If any are raised
     * this method silently ignores them. Use the {@link #tryPut} method directly if you want to catch these
     * exceptions.
     *
     * @param e The element to add.
     *
     * @throws InterruptedException If interrupted while waiting.
     */
    public void put(E e) throws InterruptedException
    {
        try
        {
            tryPut(e);
        }
        catch (SynchException ex)
        {
            // This exception is deliberately ignored. See the method comment for information about this.
        }
    }

    /**
     * Tries a synchronous put into the queue. If a consumer encounters an exception condition whilst processing the
     * data that is put, then this is returned to the caller wrapped inside a {@link SynchException}.
     *
     * @param e The data element to put into the queue. Cannot be null.
     *
     * @throws InterruptedException If the thread is interrupted whilst waiting to write to the queue or whilst waiting
     *                              on its entry in the queue being consumed.
     * @throws SynchException       If a consumer encounters an error whilst processing the data element.
     */
    public void tryPut(E e) throws InterruptedException, SynchException
    {
        if (e == null)
        {
            throw new NullPointerException();
        }

        // final Queue<E> items = this.buffer;
        final ReentrantLock lock = this.lock;
        lock.lockInterruptibly();

        try
        {
            while (count == getBufferCapacity())
            {
                // Release the lock and wait until the queue is not full.
                notFull.await();
            }
        }
        catch (InterruptedException ie)
        {
            notFull.signal(); // propagate to non-interrupted thread
            throw ie;
        }

        // There is room in the queue so insert must succeed. Insert into the queu, release the lock and block
        // the producer until its data is taken.
        insert(e, true);
    }

    /**
     * Retrieves and removes the head of this queue, waiting if no elements are present on this queue.
     * Any producer that has its data element taken by this call will be immediately unblocked. To keep the
     * producer blocked whilst taking just a single item, use the
     * {@link #drainTo(java.util.Collection<org.apache.qpid.util.concurrent.SynchRecord<E>>, int, boolean)}
     * method. There is no take method to do that because there is not usually any advantage in a synchronous hand
     * off design that consumes data one item at a time. It is normal to consume data in chunks to ammortize consumption
     * latencies accross many producers where possible.
     *
     * @return The head of this queue.
     *
     * @throws InterruptedException if interrupted while waiting.
     */
    public E take() throws InterruptedException
    {
        final ReentrantLock lock = this.lock;
        lock.lockInterruptibly();

        try
        {
            try
            {
                while (count == 0)
                {
                    // Release the lock and wait until the queue becomes non-empty.
                    notEmpty.await();
                }
            }
            catch (InterruptedException ie)
            {
                notEmpty.signal(); // propagate to non-interrupted thread
                throw ie;
            }

            // There is data in the queue so extraction must succeed. Notify any waiting threads that the queue is
            // not full, and unblock the producer that owns the data item that is taken.
            E x = extract(true, true).getElement();

            return x;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Removes all available elements from this queue and adds them into the given collection.  This operation may be
     * more efficient than repeatedly polling this queue. A failure encountered while attempting to <tt>add</tt> elements
     * to collection <tt>c</tt> may result in elements being in neither, either or both collections when the associated
     * exception is thrown. Attempts to drain a queue to itself result in <tt>IllegalArgumentException</tt>. Further,
     * the behavior of this operation is undefined if the specified collection is modified while the operation is in
     * progress.
     *
     * @param objects The collection to transfer elements into.
     *
     * @return The number of elements transferred.
     *
     * @throws NullPointerException     If objects is null.
     * @throws IllegalArgumentException If objects is this queue.
     */
    public int drainTo(Collection<? super E> objects)
    {
        return drainTo(objects, -1);
    }

    /**
     * Removes at most the given number of available elements from this queue and adds them into the given collection.
     * A failure encountered while attempting to <tt>add</tt> elements to collection <tt>c</tt> may result in elements
     * being in neither, either or both collections when the associated exception is thrown. Attempts to drain a queue
     * to itself result in <tt>IllegalArgumentException</tt>. Further, the behavior of this operation is undefined if
     * the specified collection is modified while the operation is in progress.
     *
     * @param objects     The collection to transfer elements into.
     * @param maxElements The maximum number of elements to transfer. If this is -1 then that is interpreted as meaning
     *                    all elements.
     *
     * @return The number of elements transferred.
     *
     * @throws NullPointerException     If c is null.
     * @throws IllegalArgumentException If c is this queue.
     */
    public int drainTo(Collection<? super E> objects, int maxElements)
    {
        if (objects == null)
        {
            throw new NullPointerException();
        }

        if (objects == this)
        {
            throw new IllegalArgumentException();
        }

        // final Queue<E> items = this.buffer;
        final ReentrantLock lock = this.lock;
        lock.lock();

        try
        {
            int n = 0;

            for (int max = ((maxElements >= count) || (maxElements < 0)) ? count : maxElements; n < max; n++)
            {
                // Take items from the queue, do unblock the producers, but don't send not full signals yet.
                objects.add(extract(true, false).getElement());
            }

            if (n > 0)
            {
                // count -= n;
                notFull.signalAll();
            }

            return n;
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * Takes all available data items from the queue or blocks until some become available. The returned items
     * are wrapped in a {@link SynchRecord} which provides an interface to requeue them or send errors to their
     * producers, where the producers are still blocked.
     *
     * @param c       The collection to drain the data items into.
     * @param unblock If set to <tt>true</tt> the producers for the taken items will be immediately unblocked.
     *
     * @return A count of the number of elements that were drained from the queue.
     */
    public SynchRef drainTo(Collection<SynchRecord<E>> c, boolean unblock)
    {
        return drainTo(c, -1, unblock);
    }

    /**
     * Takes up to maxElements available data items from the queue or blocks until some become available. The returned
     * items are wrapped in a {@link SynchRecord} which provides an interface to requeue them or send errors to their
     * producers, where the producers are still blocked.
     *
     * @param coll        The collection to drain the data items into.
     * @param maxElements The maximum number of elements to drain.
     * @param unblock     If set to <tt>true</tt> the producers for the taken items will be immediately unblocked.
     *
     * @return A count of the number of elements that were drained from the queue.
     */
    public SynchRef drainTo(Collection<SynchRecord<E>> coll, int maxElements, boolean unblock)
    {
        if (coll == null)
        {
            throw new NullPointerException();
        }

        // final Queue<E> items = this.buffer;
        final ReentrantLock lock = this.lock;
        lock.lock();

        try
        {
            int n = 0;

            for (int max = ((maxElements >= count) || (maxElements < 0)) ? count : maxElements; n < max; n++)
            {
                // Extract the next record from the queue, don't signall the not full condition yet and release
                // producers depending on whether the caller wants to or not.
                coll.add(extract(false, unblock));
            }

            if (n > 0)
            {
                // count -= n;
                notFull.signalAll();
            }

            return new SynchRefImpl(n, coll);
        }
        finally
        {
            lock.unlock();
        }
    }

    /**
     * This abstract method should be overriden to return an empty queue. Different implementations of producer
     * consumer buffers can control the order in which data is accessed using different queue implementations.
     * This method allows the type of queue to be abstracted out of this class and to be supplied by concrete
     * implementations.
     *
     * @return An empty queue.
     */
    protected abstract <T> Queue<T> createQueue();

    /**
     * Insert element into the queue, then possibly signal that the queue is not empty and block the producer
     * on the element until permission to procede is given.
     *
     * <p/>If the producer is to be blocked then the lock must be released first, otherwise no other process
     * will be able to get access to the queue. Hence, unlock and block are always set together.
     *
     * <p/>Call only when holding the global lock.
     *
     * @param unlockAndBlock <tt>true</tt>If the global queue lock should be released and the producer should be blocked.
     *
     * @return <tt>true</tt> if the operation succeeded, <tt>false</tt> otherwise. If the result is <tt>true</tt> this
     *         method may not return straight away, but only after the producer is unblocked by having its data
     *         consumed if the unlockAndBlock flag is set. In the false case the method will return straight away, no
     *         matter what value the unlockAndBlock flag has, leaving the global lock on.
     */
    protected boolean insert(E x, boolean unlockAndBlock)
    {
        // Create a new record for the data item.
        SynchRecordImpl<E> record = new SynchRecordImpl<E>(x);

        boolean result = buffer.offer(record);

        if (result)
        {
            count++;

            // Tell any waiting consumers that the queue is not empty.
            notEmpty.signal();

            if (unlockAndBlock)
            {
                // Allow other threads to read/write the queue.
                lock.unlock();

                // Wait until a consumer takes this data item.
                record.waitForConsumer();
            }

            return true;
        }
        else
        {
            return false;
        }
    }

    /**
     * Extract element at current take position, advance, and signal.
     *
     * <p/>Call only when holding lock.
     */
    protected SynchRecordImpl<E> extract(boolean unblock, boolean signal)
    {
        SynchRecordImpl<E> result = buffer.remove();
        count--;

        if (signal)
        {
            notFull.signal();
        }

        if (unblock)
        {
            result.releaseImmediately();
        }

        return result;
    }

    /**
     * Get the capacity of the buffer. If the buffer has no maximum capacity then Integer.MAX_VALUE is returned.
     *
     * <p/>Call only when holding lock.
     *
     * @return The maximum capacity of the buffer.
     */
    protected int getBufferCapacity()
    {
        if (buffer instanceof Capacity)
        {
            return ((Capacity) buffer).getCapacity();
        }
        else
        {
            return Integer.MAX_VALUE;
        }
    }

    /**
     * Return the head element from the buffer.
     *
     * <p/>Call only when holding lock.
     *
     * @return The head element from the buffer.
     */
    protected E peekAtBufferHead()
    {
        return buffer.peek().getElement();
    }

    public class SynchRefImpl implements SynchRef
    {
        /** Holds the number of synch records associated with this reference. */
        int numRecords;

        /** Holds a reference to the collection of synch records managed by this. */
        Collection<SynchRecord<E>> records;

        public SynchRefImpl(int n, Collection<SynchRecord<E>> records)
        {
            this.numRecords = n;
            this.records = records;
        }

        public int getNumRecords()
        {
            return numRecords;
        }

        /**
         * Any producers that have had their data elements taken from the queue but have not been unblocked are unblocked
         * when this method is called. The exception to this is producers that have had their data put back onto the queue
         * by a consumer. Producers that have had exceptions for their data items registered by consumers will be unblocked
         * but will not return from their put call normally, but with an exception instead.
         */
        public void unblockProducers()
        {
            log.debug("public void unblockProducers(): called");

            if (records != null)
            {
                for (SynchRecord<E> record : records)
                {
                    // This call takes account of items that have already been released, are to be requeued or are in
                    // error.
                    record.releaseImmediately();
                }
            }

            records = null;
        }
    }

    /**
     * A SynchRecordImpl is used by a {@link BatchSynchQueue} to pair together a producer with its data. This allows
     * the producer of data to be identified so that it can be unblocked when its data is consumed or sent errors when
     * its data cannot be consumed.
     */
    public class SynchRecordImpl<E> implements SynchRecord<E>
    {
        /** A boolean latch that determines when the producer for this data item will be allowed to continue. */
        BooleanLatch latch = new BooleanLatch();

        /** The data element associated with this item. */
        E element;

        /**
         * Create a new synch record.
         *
         * @param e The data element that the record encapsulates.
         */
        public SynchRecordImpl(E e)
        {
            // Keep the data element.
            element = e;
        }

        /**
         * Waits until the producer is given permission to proceded by a consumer.
         */
        public void waitForConsumer()
        {
            latch.await();
        }

        /**
         * Gets the data element contained by this record.
         *
         * @return The data element contained by this record.
         */
        public E getElement()
        {
            return element;
        }

        /**
         * Immediately releases the producer of this data record. Consumers can bring the synchronization time of
         * producers to a minimum by using this method to release them at the earliest possible moment when batch
         * consuming records from sychronized producers.
         */
        public void releaseImmediately()
        {
            // Check that the record has not already been released, is in error or is to be requeued.
            latch.signal();

            // Propagate errors to the producer.

            // Requeue items to be requeued.
        }

        /**
         * Tells the synch queue to put this element back onto the queue instead of releasing its producer.
         * The element is not requeued immediately but upon calling the {@link SynchRef#unblockProducers()} method or
         * the {@link #releaseImmediately()} method.
         *
         * <p/>This method will raise a runtime exception {@link AlreadyUnblockedException} if the producer for this
         * element has already been unblocked.
         */
        public void reQueue()
        {
            throw new RuntimeException("Not implemented.");
        }

        /**
         * Tells the synch queue to raise an exception with this elements producer. The exception is not raised
         * immediately but upon calling the {@link SynchRef#unblockProducers()} method or the
         * {@link #releaseImmediately()} method. The exception will be wrapped in a {@link SynchException} before it is
         * raised on the producer.
         *
         * <p/>This method is unusual in that it accepts an exception as an argument. This is non-standard but is used
         * because the exception is to be passed onto a different thread.
         *
         * <p/>This method will raise a runtime exception {@link AlreadyUnblockedException} if the producer for this
         * element has already been unblocked.
         *
         * @param e The exception to raise on the producer.
         */
        public void inError(Exception e)
        {
            throw new RuntimeException("Not implemented.");
        }
    }
}
