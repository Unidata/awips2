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


import java.util.Collection;
import java.util.concurrent.BlockingQueue;

/**
 * BatchSynchQueue is an abstraction of the classic producer/consumer buffer pattern for thread interaction. In this
 * pattern threads can deposit data onto a buffer whilst other threads take data from the buffer and perform usefull
 * work with it. A BatchSynchQueue adds to this the possibility that producers can be blocked until their data is
 * consumed or until a consumer chooses to release the producer some time after consuming the data from the queue.
 *
 * <p>There are a number of possible advantages to using this technique when compared with having the producers
 * processing their own data:
 *
 * <ul>
 * <li>Data may be deposited asynchronously in the buffer allowing the producers to continue running.</li>
 * <li>Data may be deposited synchronously in the buffer so that producers wait until their data has been processed
 *     before being allowed to continue.</li>
 * <li>Variable rates of production/consumption can be smoothed over by the buffer as it provides space in memory to
 *     hold data between production and consumption.</li>
 * <li>Consumers may be able to batch data as they consume it leading to more efficient consumption over
 *     individual data item consumption where latency associated with the consume operation can be ammortized.
 *     For example, it may be possibly to ammortize the cost of a disk seek over many producers.</li>
 * <li>Data from seperate threads can be combined together in the buffer, providing a convenient way of spreading work
 *     amongst many workers and gathering the results together again.</li>
 * <li>Different types of queue can be used to hold the buffer, resulting in different processing orders. For example,
 *     lifo, fifo, priority heap, etc.</li>
 * </ul>
 *
 * <p/>The asynchronous type of producer/consumer buffers is already well supported by the java.util.concurrent package
 * (in Java 5) and there is also a synchronous queue implementation available there too. This interface extends the
 * blocking queue with some more methods for controlling a synchronous blocking queue. In particular it adds additional
 * take methods that can be used to take data from a queue without releasing producers, so that consumers have an
 * opportunity to confirm correct processing of the data before producers are released. It also adds a put method with
 * exceptions so that consumers can signal exception cases back to producers where there are errors in the data.
 *
 * <p/>This type of queue is usefull in situations where consumers can obtain an efficiency gain by batching data
 * from many threads but where synchronous handling of that data is neccessary because producers need to know that
 * their data has been processed before they continue. For example, sending a bundle of messages together, or writing
 * many records to disk at once, may result in improved performance but the originators of the messages or disk records
 * need confirmation that their data has really been sent or saved to disk.
 *
 * <p/>The consumer can put an element back onto the queue or send an error message to the elements producer using the
 * {@link SynchRecord} interface.
 *
 * <p/>The {@link #take()}, {@link #drainTo(java.util.Collection<? super E>)}  and
 * {@link #drainTo(java.util.Collection<? super E>, int)} methods from {@link BlockingQueue} should behave as if they
 * have been called with unblock set to false. That is they take elements from the queue but leave the producers
 * blocked. These methods do not return collections of {@link SynchRecord}s so they do not supply an interface through
 * which errors or re-queuings can be applied. If these methods are used then the consumer must succesfully process
 * all the records it takes.
 *
 * <p/>The {@link #put} method should silently swallow any exceptions that consumers attempt to return to the caller.
 * In order to handle exceptions the {@link #tryPut} method must be used.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Handle synchronous puts, with possible exceptions.
 * <tr><td> Allow consumers to take many records from a queue in a batch.
 * <tr><td> Allow consumers to decide when to unblock synchronous producers.
 * </table>
 */
public interface BatchSynchQueue<E> extends BlockingQueue<E>
{
    /**
     * Tries a synchronous put into the queue. If a consumer encounters an exception condition whilst processing the
     * data that is put, then this is returned to the caller wrapped inside a {@link SynchException}.
     *
     * @param e The data element to put into the queue.
     *
     * @throws InterruptedException If the thread is interrupted whilst waiting to write to the queue or whilst waiting
     *                              on its entry in the queue being consumed.
     * @throws SynchException       If a consumer encounters an error whilst processing the data element.
     */
    public void tryPut(E e) throws InterruptedException, SynchException;

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
    public SynchRef drainTo(Collection<SynchRecord<E>> c, boolean unblock);

    /**
     * Takes up to maxElements available data items from the queue or blocks until some become available. The returned
     * items are wrapped in a {@link SynchRecord} which provides an interface to requeue them or send errors to their
     * producers, where the producers are still blocked.
     *
     * @param c           The collection to drain the data items into.
     * @param maxElements The maximum number of elements to drain.
     * @param unblock     If set to <tt>true</tt> the producers for the taken items will be immediately unblocked.
     *
     * @return A count of the number of elements that were drained from the queue.
     */
    public SynchRef drainTo(Collection<SynchRecord<E>> c, int maxElements, boolean unblock);
}
