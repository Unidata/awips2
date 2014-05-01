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
package org.apache.qpid.server.exchange;

import org.apache.qpid.AMQException;
import org.apache.qpid.server.queue.NoConsumersException;
import org.apache.qpid.server.util.TimedRun;
import org.apache.qpid.server.util.AveragedRun;
import org.apache.qpid.framing.BasicPublishBody;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.ContentBody;

import java.util.List;

/**
 * Want to vary the number of regsitrations, messages and matches and measure
 * the corresponding variance in execution time.
 * <p/>
 * Each registration will contain the 'All' header, even registrations will
 * contain the 'Even' header and odd headers will contain the 'Odd' header.
 * In additions each regsitration will have a unique value for the 'Specific'
 * header as well.
 * <p/>
 * Messages can then be routed to all registrations, to even- or odd- registrations
 * or to a specific registration.
 *
 */
public class HeadersExchangePerformanceTest extends AbstractHeadersExchangeTest
{
    private static enum Mode {ALL, ODD_OR_EVEN, SPECIFIC}

    private final TestQueue[] queues;
    private final Mode mode;

    public HeadersExchangePerformanceTest(Mode mode, int registrations) throws AMQException
    {
        this.mode = mode;
        queues = new TestQueue[registrations];
        for (int i = 0; i < queues.length; i++)
        {
            switch(mode)
            {
                case ALL:
                    queues[i] = bind(new FastQueue("Queue" + i), "All");
                    break;
                case ODD_OR_EVEN:
                    queues[i] = bind(new FastQueue("Queue" + i), "All", oddOrEven(i));
                    break;
                case SPECIFIC:
                    queues[i] = bind(new FastQueue("Queue" + i), "All", oddOrEven(i), "Specific"+ i);
                    break;
            }
        }
    }

    void sendToAll(int count) throws AMQException
    {
        send(count, "All=True");
    }

    void sendToOdd(int count) throws AMQException
    {
        send(count, "All=True", "Odd=True");
    }

    void sendToEven(int count) throws AMQException
    {
        send(count, "All=True", "Even=True");
    }

    void sendToAllSpecifically(int count) throws AMQException
    {
        for (int i = 0; i < queues.length; i++)
        {
            sendToSpecific(count, i);
        }
    }

    void sendToSpecific(int count, int index) throws AMQException
    {
        send(count, "All=True", oddOrEven(index) + "=True", "Specific=" + index);
    }

    private void send(int count, String... headers) throws AMQException
    {
        for (int i = 0; i < count; i++)
        {
            route(new Message("Message" + i, headers));
        }
    }

    private static String oddOrEven(int i)
    {
        return (i % 2 == 0 ? "Even" : "Odd");
    }

    static class FastQueue extends TestQueue
    {

        public FastQueue(String name) throws AMQException
        {
            super(name);
        }

        public void deliver(BasicPublishBody publishBody, ContentHeaderBody contentHeaderBody, List<ContentBody> contentBodies) throws NoConsumersException
        {
            //just discard as we are not testing routing functionality here
        }
    }

    static class Test extends TimedRun
    {
        private final Mode mode;
        private final int registrations;
        private final int count;
        private HeadersExchangePerformanceTest test;

        Test(Mode mode, int registrations, int count)
        {
            super(mode + ", registrations=" + registrations + ", count=" + count);
            this.mode = mode;
            this.registrations = registrations;
            this.count = count;
        }

        protected void setup() throws Exception
        {
            test = new HeadersExchangePerformanceTest(mode, registrations);
            run(100); //do a warm up run before times start
        }

        protected void teardown() throws Exception
        {
            test = null;
            System.gc();
        }

        protected void run() throws Exception
        {
            run(count);
        }

        private void run(int count) throws Exception
        {
            switch(mode)
            {
                case ALL:
                    test.sendToAll(count);
                    break;
                default:
                    System.out.println("Test for " + mode + " not yet implemented.");
            }
        }
    }

    public static void main(String[] argv) throws Exception
    {
        int registrations = Integer.parseInt(argv[0]);
        int messages = Integer.parseInt(argv[1]);
        int iterations = Integer.parseInt(argv[2]);
        TimedRun test = new Test(Mode.ALL, registrations, messages);
        AveragedRun tests = new AveragedRun(test, iterations);
        System.out.println(tests.call());
    }
}

