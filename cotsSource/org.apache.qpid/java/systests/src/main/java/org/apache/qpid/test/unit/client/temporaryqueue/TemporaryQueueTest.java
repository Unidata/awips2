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

package org.apache.qpid.test.unit.client.temporaryqueue;

import javax.jms.Connection;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TemporaryQueue;
import javax.jms.TextMessage;
import junit.framework.Assert;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.jms.ConnectionListener;

import java.util.ArrayList;
import java.util.List;
import java.util.LinkedList;

public class TemporaryQueueTest extends QpidTestCase implements ExceptionListener
{
    private List<Exception> _exceptions = new ArrayList<Exception>();

    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
       super.tearDown();
    }

    protected Connection createConnection() throws Exception
    {
        return  getConnection("guest", "guest");
    }

    public void testTemporaryQueue() throws Exception
    {
        Connection conn = createConnection();
        Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
        TemporaryQueue queue = session.createTemporaryQueue();
        assertNotNull(queue);
        MessageProducer producer = session.createProducer(queue);
        MessageConsumer consumer = session.createConsumer(queue);
        conn.start();
        producer.send(session.createTextMessage("hello"));
        TextMessage tm = (TextMessage) consumer.receive(2000);
        assertNotNull(tm);
        assertEquals("hello", tm.getText());

        try
        {
            queue.delete();
            fail("Expected JMSException : should not be able to delete while there are active consumers");
        }
        catch (JMSException je)
        {
            ; //pass
        }

        consumer.close();

        try
        {
            queue.delete();
        }
        catch (JMSException je)
        {
            fail("Unexpected Exception: " + je.getMessage());
        }

        conn.close();
    }

    public void tUniqueness() throws Exception
    {
        int numProcs = Runtime.getRuntime().availableProcessors();
        final int threadsProc = 5;

        runUniqueness(1, 10);
        runUniqueness(numProcs * threadsProc, 10);
        runUniqueness(numProcs * threadsProc, 100);
        runUniqueness(numProcs * threadsProc, 500);
    }

    void runUniqueness(int makers, int queues) throws Exception
    {
        Connection connection = createConnection();

        Session session = connection.createSession(false, Session.CLIENT_ACKNOWLEDGE);

        List<TempQueueMaker> tqList = new LinkedList<TempQueueMaker>();

        //Create Makers
        for (int m = 0; m < makers; m++)
        {
            tqList.add(new TempQueueMaker(session, queues));
        }


        List<Thread> threadList = new LinkedList<Thread>();

        //Create Makers
        for (TempQueueMaker maker : tqList)
        {
            threadList.add(new Thread(maker));
        }

        //Start threads
        for (Thread thread : threadList)
        {
            thread.start();
        }

        // Join Threads
        for (Thread thread : threadList)
        {
            try
            {
                thread.join();
            }
            catch (InterruptedException e)
            {
                fail("Couldn't correctly join threads");
            }
        }


        List<AMQQueue> list = new LinkedList<AMQQueue>();

        // Test values
        for (TempQueueMaker maker : tqList)
        {
            check(maker, list);
        }

        Assert.assertEquals("Not enough queues made.", makers * queues, list.size());

        connection.close();
    }

    private void check(TempQueueMaker tq, List<AMQQueue> list)
    {
        for (AMQQueue q : tq.getList())
        {
            if (list.contains(q))
            {
                fail(q + " already exists.");
            }
            else
            {
                list.add(q);
            }
        }
    }


    class TempQueueMaker implements Runnable
    {
        List<AMQQueue> _queues;
        Session _session;
        private int _count;


        TempQueueMaker(Session session, int queues) throws JMSException
        {
            _queues = new LinkedList<AMQQueue>();

            _count = queues;

            _session = session;
        }

        public void run()
        {
            int i = 0;
            try
            {
                for (; i < _count; i++)
                {
                    _queues.add((AMQQueue) _session.createTemporaryQueue());
                }
            }
            catch (JMSException jmse)
            {
                //stop
            }
        }

        List<AMQQueue> getList()
        {
            return _queues;
        }
    }

    public void testQPID1217() throws Exception
    {
        Connection conA = getConnection();
        conA.setExceptionListener(this);
        Session sessA = conA.createSession(false, Session.AUTO_ACKNOWLEDGE);
        TemporaryQueue temp = sessA.createTemporaryQueue();
        
        MessageProducer prod = sessA.createProducer(temp);
        prod.send(sessA.createTextMessage("hi"));

        Thread.sleep(500);
        assertTrue("Exception received", _exceptions.isEmpty());
        
        Connection conB = getConnection();
        Session sessB = conB.createSession(false, Session.AUTO_ACKNOWLEDGE);
        
        JMSException ex = null;
        try
        {
            MessageConsumer consB = sessB.createConsumer(temp);
        } 
        catch (JMSException e)
        {
            ex = e; 
        }
        assertNotNull(ex);
    }
    
    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(TemporaryQueueTest.class);
    }

    public void onException(JMSException arg0)
    {
        _exceptions.add(arg0);
    }

}
