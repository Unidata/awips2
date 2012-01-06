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
package org.apache.qpid.server.logging.actors;

import org.apache.qpid.AMQException;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.logging.LogMessage;
import org.apache.qpid.server.logging.LogSubject;
import org.apache.qpid.server.registry.ApplicationRegistry;

/**
 * Test : CurrentActorTest
 * Summary:
 * Validate ThreadLocal operation.
 *
 * Test creates THREADS number of threads which all then execute the same test
 * together ( as close as looping Thread.start() will allow).
 *
 * Test:
 * Test sets the CurrentActor then proceeds to retrieve the value and use it.
 *
 * The test also validates that it is the same LogActor that this thread set.
 *
 * Finally the LogActor is removed and tested to make sure that it was
 * successfully removed.
 *
 * By having a higher number of threads than would normally be used in the
 * Poolling filter we aim to catch the race condition where a ThreadLocal remove
 * is called before one or more threads call get(). This way we can ensure that
 * the remove does not affect more than the Thread it was called in.
 */
public class CurrentActorTest extends BaseConnectionActorTestCase
{
    //Set this to be a reasonably large number
    int THREADS = 10;

    // Record any exceptions that are thrown by the threads
    Exception[] _errors = new Exception[THREADS];

    /**
     * Test that CurrentActor behaves as LIFO queue.
     *
     * Test creates two Actors Connection and Channel and then sets the
     * CurrentActor.
     *
     * The test validates that CurrentActor remembers the Connection actor
     * after the Channel actor has been removed.
     *
     * And then finally validates that removing the Connection actor results
     * in there being no actors set.
     *
     * @throws AMQException
     */
    public void testLIFO() throws AMQException
    {
        // Create a new actor using retrieving the rootMessageLogger from
        // the default ApplicationRegistry.
        //fixme reminder that we need a better approach for broker testing.
        AMQPConnectionActor connectionActor = new AMQPConnectionActor(_session,
                                                                      ApplicationRegistry.getInstance().
                                                                              getRootMessageLogger());

        /*
         * Push the actor on to the stack:
         *
         *  CurrentActor -> Connection
         *       Stack   -> null
         */
        CurrentActor.set(connectionActor);

        //Use the Actor to send a simple message
        CurrentActor.get().message(new LogSubject()
        {
            public String toString()
            {
                return "[CurrentActorTest] ";
            }

        }, new LogMessage()
        {
            public String toString()
            {
                return "Connection Log Msg";
            }
        });

        // Verify it was the same actor as we set earlier
        assertEquals("Retrieved actor is not as expected ",
                     connectionActor, CurrentActor.get());

        /**
         * Set the actor to now be the Channel actor so testing the ability
         * to push the actor on to the stack:
         *
         *  CurrentActor -> Channel
         *       Stack   -> Connection, null
         *
         */

        AMQChannel channel = new AMQChannel(_session, 1, _session.getVirtualHost().getMessageStore());

        AMQPChannelActor channelActor = new AMQPChannelActor(channel,
                                                             ApplicationRegistry.getInstance().
                                                                     getRootMessageLogger());

        CurrentActor.set(channelActor);

        //Use the Actor to send a simple message
        CurrentActor.get().message(new LogSubject()
        {
            public String toString()
            {
                return "[CurrentActorTest] ";
            }

        }, new LogMessage()
        {
            public String toString()
            {
                return "Channel Log Msg";
            }
        });

        // Verify it was the same actor as we set earlier
        assertEquals("Retrieved actor is not as expected ",
                     channelActor, CurrentActor.get());

        // Remove the ChannelActor from the stack
        CurrentActor.remove();
        /*
         * Pop the actor on to the stack:
         *
         *  CurrentActor -> Connection
         *       Stack   -> null
         */


        // Verify we now have the same connection actor as we set earlier
        assertEquals("Retrieved actor is not as expected ",
                     connectionActor, CurrentActor.get());

        // Verify that removing the our last actor it returns us to the test
        // default that the ApplicationRegistry sets.
        CurrentActor.remove();
        /*
         * Pop the actor on to the stack:
         *
         *  CurrentActor -> null
         */


        assertEquals("CurrentActor not the Test default", TestLogActor.class ,CurrentActor.get().getClass());
    }

    /**
     * Test the setting CurrentActor is done correctly as a ThreadLocal.
     *
     * The test starts 'THREADS' threads that all set the CurrentActor log
     * a message then remove the actor.
     *
     * Checks are done to ensure that there is no set actor after the remove.
     *
     * If the ThreadLoacl was not working then having concurrent actor sets
     * would result in more than one actor and so the remove will not result
     * in the clearing of the CurrentActor
     *
     */
    public void testThreadLocal()
    {

        new Runnable(){
            public void run()
            {
                System.out.println(_errors[0]);
            }
        };

        // Setup the threads
        Thread[] threads = new Thread[THREADS];
        for (int count = 0; count < THREADS; count++)
        {
            Runnable test = new LogMessagesWithAConnectionActor(count);
            threads[count] = new Thread(test);
        }

        //Run the threads
        for (int count = 0; count < THREADS; count++)
        {
            threads[count].start();
        }

        // Wait for them to finish
        for (int count = 0; count < THREADS; count++)
        {
            try
            {
                threads[count].join();
            }
            catch (InterruptedException e)
            {
                //if we are interrupted then we will exit shortly.
            }
        }

        // Verify that none of the tests threw an exception
        for (int count = 0; count < THREADS; count++)
        {
            if (_errors[count] != null)
            {
                _errors[count].printStackTrace();
                fail("Error occured in thread:" + count);
            }
        }
    }

    /**
     * Creates a new ConnectionActor and logs the given number of messages
     * before removing the actor and validating that there is no set actor.
     */
    public class LogMessagesWithAConnectionActor implements Runnable
    {
        int count;

        LogMessagesWithAConnectionActor(int count)
        {
            this.count = count;
        }

        public void run()
        {

            // Create a new actor using retrieving the rootMessageLogger from
            // the default ApplicationRegistry.
            //fixme reminder that we need a better approach for broker testing.
            AMQPConnectionActor actor = new AMQPConnectionActor(_session,
                                                                ApplicationRegistry.getInstance().
                                                                        getRootMessageLogger());

            CurrentActor.set(actor);

            try
            {
                //Use the Actor to send a simple message
                CurrentActor.get().message(new LogSubject()
                {
                    public String toString()
                    {
                        return "[CurrentActorTest] ";
                    }

                }, new LogMessage()
                {
                    public String toString()
                    {
                        return "Running Thread:" + count;
                    }
                });

                // Verify it was the same actor as we set earlier
                assertEquals("Retrieved actor is not as expected ",
                             actor, CurrentActor.get());

                // Verify that removing the actor works for this thread
                CurrentActor.remove();

                assertNull("CurrentActor should be null", CurrentActor.get());
            }
            catch (Exception e)
            {
                _errors[count] = e;
            }

        }
    }

}
