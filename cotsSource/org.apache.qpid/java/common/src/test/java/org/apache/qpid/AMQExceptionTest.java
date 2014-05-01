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
package org.apache.qpid;

import junit.framework.TestCase;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.framing.AMQFrameDecodingException;

/**
 * This test is to ensure that when an AMQException is rethrown that the specified exception is correctly wrapped up.
 *
 * There are three cases:
 * Re-throwing an AMQException
 * Re-throwing a Subclass of AMQException
 * Re-throwing a Subclass of AMQException that does not have the default AMQException constructor which will force the
 * creation of an AMQException.
 */
public class AMQExceptionTest extends TestCase
{
    /**
     * Test that an AMQException will be correctly created and rethrown.
     */
    public void testRethrowGeneric()
    {
        AMQException test = new AMQException(AMQConstant.ACCESS_REFUSED, "refused", new RuntimeException());

        AMQException e = reThrowException(test);

        assertEquals("Exception not of correct class", AMQException.class, e.getClass());

    }

    /**
     * Test that a subclass of AMQException that has the default constructor will be correctly created and rethrown.
     */
    public void testRethrowAMQESubclass()
    {
        AMQFrameDecodingException test = new AMQFrameDecodingException(AMQConstant.INTERNAL_ERROR,
                                                                       "Error",
                                                                       new Exception());
        AMQException e = reThrowException(test);

        assertEquals("Exception not of correct class", AMQFrameDecodingException.class, e.getClass());
    }

    /**
     * Test that a subclass of AMQException that doesnot have the  default constructor will be correctly rethrown as an
     * AMQException
     */
    public void testRethrowAMQESubclassNoConstructor()
    {
        AMQExceptionSubclass test = new AMQExceptionSubclass("Invalid Argument Exception");

        AMQException e = reThrowException(test);

        assertEquals("Exception not of correct class", AMQException.class, e.getClass());
    }

    /**
     * Private method to rethrown and validate the basic values of the rethrown
     * @param test Exception to rethrow
     * @throws AMQException the rethrown exception
     */
    private AMQException reThrowException(AMQException test)
    {
        AMQException amqe = test.cloneForCurrentThread();

        assertEquals("Error code does not match.", test.getErrorCode(), amqe.getErrorCode());
        assertTrue("Exception message does not start as expected.", amqe.getMessage().startsWith(test.getMessage()));
        assertEquals("Test Exception is not set as the cause", test, amqe.getCause());
        assertEquals("Cause is not correct", test.getCause(), amqe.getCause().getCause());

        return amqe;
    }

    /**
     * Private class that extends AMQException but does not have a default exception.
     */
    private class AMQExceptionSubclass extends AMQException
    {

        public AMQExceptionSubclass(String msg)
        {
            super(null, msg, null);
        }
    }
}

