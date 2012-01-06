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

import junit.framework.TestCase;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.AMQBody;
import org.apache.qpid.framing.AMQMethodBody;
import org.apache.qpid.framing.amqp_8_0.BasicRecoverOkBodyImpl;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.transport.TestNetworkDriver;
import org.apache.qpid.client.MockAMQConnection;
import org.apache.qpid.client.AMQAuthenticationException;
import org.apache.qpid.client.state.AMQState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * This is a test address QPID-1431 where frame listeners would fail to be notified of an incomming exception.
 *
 * Currently we do checks at the Session level to ensure that the connection/session are open. However, it is possible
 * for the connection to close AFTER this check has been performed.
 *
 * Performing a similar check at the frameListener level in AMQProtocolHandler makes most sence as this will prevent
 * listening when there can be no returning frames.
 *
 * With the correction in place it also means that the new listener will either make it on to the list for notification
 * or it will be notified of any existing exception due to the connection being closed.
 *
 * There may still be an issue in this space if the client utilises a second thread to close the session as there will
 * be no exception set to throw and so the wait will occur. That said when the session is closed the framelisteners
 * should be notified. Not sure this is tested.
 */
public class AMQProtocolHandlerTest extends TestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(AMQProtocolHandlerTest.class);

    // The handler to test
    AMQProtocolHandler _handler;

    // A frame to block upon whilst waiting the exception
    AMQFrame _blockFrame;

    // Latch to know when the listener receives an exception
    private CountDownLatch _handleCountDown;
    // The listener that will receive an exception
    BlockToAccessFrameListener _listener;

    @Override
    public void setUp() throws Exception
    {
        //Create a new ProtocolHandler with a fake connection.
        _handler = new AMQProtocolHandler(new MockAMQConnection("amqp://guest:guest@client/test?brokerlist='vm://:1'"));
        _handler.setNetworkDriver(new TestNetworkDriver());
         AMQBody body = BasicRecoverOkBodyImpl.getFactory().newInstance(null, 1);
        _blockFrame = new AMQFrame(0, body);

        _handleCountDown = new CountDownLatch(1);

        _logger.info("Creating _Listener that should also receive the thrown exception.");
        _listener = new BlockToAccessFrameListener(1);
    }

    /**
     * There are two paths based on the type of exception thrown.
     *
     * This tests that when an AMQException is thrown we get the same type of AMQException back with the real exception
     * wrapped as the cause.
     *
     * @throws InterruptedException  - if we are unable to wait for the test signals
     */
    public void testFrameListenerUpdateWithAMQException() throws InterruptedException
    {
        AMQException trigger = new AMQAuthenticationException(AMQConstant.ACCESS_REFUSED,
                                                              "AMQPHTest", new RuntimeException());

        performWithException(trigger);


        AMQException receivedException = (AMQException) _listener.getReceivedException();

        assertEquals("Return exception was not the expected type",
                     AMQAuthenticationException.class, receivedException.getClass());

        assertEquals("The _Listener did not receive the correct error code",
                     trigger.getErrorCode(), receivedException.getErrorCode());
    }

    /**
     * There are two paths based on the type of exception thrown.
     *
     * This tests that when a generic Exception is thrown we get the exception back wrapped in a AMQException
     * as the cause.
     * @throws InterruptedException  - if we are unable to wait for the test signals
     */
    public void testFrameListenerUpdateWithException() throws InterruptedException
    {

        Exception trigger = new Exception(new RuntimeException());

        performWithException(trigger);

        assertEquals("The _Listener did not receive the correct error code",
                     AMQConstant.INTERNAL_ERROR,  ((AMQException)_listener.getReceivedException()).getErrorCode());
    }

    /**
     * This is the main test method for both test cases.
     *
     * What occurs is that we create a new thread that will block (<30s[DEFAULT]) for a frame or exception to occur .
     *
     * We use a CountDownLatch to ensure that the new thread is running before we then yield and sleep to help ensure
     * the new thread has entered the synchronized block in the writeCommandFrameAndWaitForReply.
     *
     * We can then ack like an the incomming exception handler in (ConnectionCloseMethodHandler).
     *
     * We fire the error to the stateManager, which in this case will recored the error as there are no state listeners.
     *
     * We then set the connection to be closed, as we would normally close the socket at this point.
     *
     * Then fire the exception in to any frameListeners.
     *
     * The blocked listener (created above) when receiving the error simulates the user by creating a new request to
     * block for a frame.
     *
     * This request should fail. Prior to the fix this will fail with a NPE as we are attempting to use a null listener
     * in the writeCommand.... call L:268.
     *
     * This highlights that the listener would be added dispite there being a pending error state that the listener will
     * miss as it is not currently part of the _frameListeners set that is being notified by the iterator.
     *
     * The method waits to ensure that an exception is received before returning.
     *
     * The calling methods validate that exception that was received based on the one they sent in.
     *
     * @param trigger The exception to throw through the handler
     */
    private void performWithException(Exception trigger) throws InterruptedException
    {

        final CountDownLatch callingWriteCommand = new CountDownLatch(1);

        //Set an initial listener that will allow us to create a new blocking method
        new Thread(new Runnable()
        {
            public void run()
            {

                try
                {

                    _logger.info("At initial block, signalling to fire new exception");
                    callingWriteCommand.countDown();

                    _handler.writeCommandFrameAndWaitForReply(_blockFrame, _listener);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail(e.getMessage());
                }
            }
        }).start();

        _logger.info("Waiting for 'initial block' to start ");
        if (!callingWriteCommand.await(1000, TimeUnit.MILLISECONDS))
        {
            fail("Failed to start new thread to block for frame");
        }

        // Do what we can to ensure that this thread does not continue before the above thread has hit the synchronized
        // block in the writeCommandFrameAndWaitForReply
        Thread.yield();
        Thread.sleep(1000);

        _logger.info("Firing Erorr through state manager. There should be not state waiters here.");
        _handler.getStateManager().error(trigger);

        _logger.info("Setting state to be CONNECTION_CLOSED.");

        _handler.getStateManager().changeState(AMQState.CONNECTION_CLOSED);

        _logger.info("Firing exception");
        _handler.propagateExceptionToFrameListeners(trigger);

        _logger.info("Awaiting notifcation from handler that exception arrived.");

        if (!_handleCountDown.await(2000, TimeUnit.MILLISECONDS))
        {
            fail("Failed to handle exception and timeout has not occured");
        }


        assertNotNull("The _Listener did not receive the exception", _listener.getReceivedException());

        assertTrue("Received exception not an AMQException",
                      _listener.getReceivedException() instanceof AMQException);

        AMQException receivedException = (AMQException) _listener.getReceivedException();

        assertTrue("The _Listener did not receive the correct message",
                   receivedException.getMessage().startsWith(trigger.getMessage()));


        assertEquals("The _Listener did not receive the correct cause",
                     trigger, receivedException.getCause());

        assertEquals("The _Listener did not receive the correct sub cause",
                     trigger.getCause(), receivedException.getCause().getCause());

    }

    class BlockToAccessFrameListener extends BlockingMethodFrameListener
    {
        private Exception _receivedException = null;

        /**
         * Creates a new method listener, that filters incoming method to just those that match the specified channel id.
         *
         * @param channelId The channel id to filter incoming methods with.
         */
        public BlockToAccessFrameListener(int channelId)
        {
            super(channelId);
            _logger.info("Creating a listener:" + this);
        }

        public boolean processMethod(int channelId, AMQMethodBody frame)
        {
            return true;
        }

        @Override
        public void error(Exception e)
        {
            _logger.info("Exception(" + e + ") Received by:" + this);
            // Create a new Thread to start the blocking registration.
            new Thread(new Runnable()
            {

                public void run()
                {
                    //Set an initial listener that will allow us to create a new blocking method
                    try
                    {
                        _handler.writeCommandFrameAndWaitForReply(_blockFrame, null, 2000L);
                        _logger.info("listener(" + this + ") Wait completed");
                    }
                    catch (Exception e)
                    {
                        _logger.info("listener(" + this + ") threw exception:" + e.getMessage());
                        _receivedException = e;
                    }

                    _logger.info("listener(" + this + ") completed");
                    _handleCountDown.countDown();
                }
            }).start();
        }

        public Exception getReceivedException()
        {
            return _receivedException;
        }
    }

}
