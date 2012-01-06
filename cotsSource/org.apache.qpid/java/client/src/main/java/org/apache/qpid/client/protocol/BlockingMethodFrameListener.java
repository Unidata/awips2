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
package org.apache.qpid.client.protocol;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.qpid.AMQException;
import org.apache.qpid.AMQTimeoutException;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.util.BlockingWaiter;
import org.apache.qpid.framing.AMQMethodBody;
import org.apache.qpid.protocol.AMQMethodEvent;
import org.apache.qpid.protocol.AMQMethodListener;

/**
 * BlockingMethodFrameListener is a 'rendezvous' which acts as a {@link AMQMethodListener} that delegates handling of
 * incoming methods to a method listener implemented as a sub-class of this and hands off the processed method or
 * error to a consumer. The producer of the event does not have to wait for the consumer to take the event, so this
 * differs from a 'rendezvous' in that sense.
 *
 * <p/>BlockingMethodFrameListeners are used to coordinate waiting for replies to method calls that expect a response.
 * They are always used in a 'one-shot' manner, that is, to recieve just one response. Usually the caller has to register
 * them as method listeners with an event dispatcher and remember to de-register them (in a finally block) once they
 * have been completed.
 *
 * <p/>The {@link #processMethod} must return <tt>true</tt> on any incoming method that it handles. This indicates to
 * this listeners that the method it is waiting for has arrived. Incoming methods are also filtered by channel prior to
 * being passed to the {@link #processMethod} method, so responses are only received for a particular channel. The
 * channel id must be passed to the constructor.
 *
 * <p/>Errors from the producer are rethrown to the consumer.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Accept notification of AMQP method events. <td> {@link AMQMethodEvent}
 * <tr><td> Delegate handling of the method to another method listener. <td> {@link AMQMethodBody}
 * <tr><td> Block until a method is handled by the delegated to handler.
 * <tr><td> Propagate the most recent exception to the consumer.
 * </table>
 *
 * @todo Might be neater if this method listener simply wrapped another that provided the method handling using a
 * methodRecevied method. The processMethod takes an additional channelId, however none of the implementations
 * seem to use it. So wrapping the listeners is possible.
 * @todo If the retrotranslator can handle it, could use a SynchronousQueue to implement this rendezvous. Need to
 * check that SynchronousQueue has a non-blocking put method available.
 */
public abstract class BlockingMethodFrameListener extends BlockingWaiter<AMQMethodEvent> implements AMQMethodListener
{

    /** Holds the channel id for the channel upon which this listener is waiting for a response. */
    protected int _channelId;

    /**
     * Creates a new method listener, that filters incoming method to just those that match the specified channel id.
     *
     * @param channelId The channel id to filter incoming methods with.
     */
    public BlockingMethodFrameListener(int channelId)
    {
        _channelId = channelId;
    }

    /**
     * Delegates any additional handling of the incoming methods to another handler.
     *
     * @param channelId The channel id of the incoming method.
     * @param frame     The method body.
     *
     * @return <tt>true</tt> if the method was handled, <tt>false</tt> otherwise.
     */
    public abstract boolean processMethod(int channelId, AMQMethodBody frame);

    public boolean process(AMQMethodEvent evt)
    {
        AMQMethodBody method = evt.getMethod();

        return (evt.getChannelId() == _channelId) && processMethod(evt.getChannelId(), method);
    }

    /**
     * Informs this listener that an AMQP method has been received.
     *
     * @param evt The AMQP method.
     *
     * @return <tt>true</tt> if this listener has handled the method, <tt>false</tt> otherwise.
     */
    public boolean methodReceived(AMQMethodEvent evt)
    {
        return received(evt);
    }

    /**
     * Blocks until a method is received that is handled by the delegated to method listener, or the specified timeout
     * has passed.
     *
     * @param timeout The timeout in milliseconds.
     *
     * @return The AMQP method that was received.
     *
     * @throws AMQException
     * @throws FailoverException
     */
    public AMQMethodEvent blockForFrame(long timeout) throws AMQException, FailoverException
    {
        try
        {
            return (AMQMethodEvent) block(timeout);
        }
        finally
        {
            //Prevent any more errors being notified to this waiter.
            close();
        }
    }

}
