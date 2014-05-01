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
package org.apache.qpid.transport;

import org.apache.mina.util.AvailablePortFinder;

import org.apache.qpid.util.concurrent.Condition;

import org.apache.qpid.transport.network.ConnectionBinding;
import org.apache.qpid.transport.network.io.IoAcceptor;
import org.apache.qpid.transport.util.Logger;
import org.apache.qpid.transport.util.Waiter;

import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.io.IOException;

import static org.apache.qpid.transport.Option.*;

/**
 * ConnectionTest
 */

public class ConnectionTest extends TestCase implements SessionListener
{

    private static final Logger log = Logger.get(ConnectionTest.class);

    private int port;
    private volatile boolean queue = false;
    private List<MessageTransfer> messages = new ArrayList<MessageTransfer>();
    private List<MessageTransfer> incoming = new ArrayList<MessageTransfer>();

    private IoAcceptor _ioa = null;


    protected void setUp() throws Exception
    {
        super.setUp();

        port = AvailablePortFinder.getNextAvailable(12000);
    }

    protected void tearDown() throws Exception
    {
        if (_ioa != null)
        {
            _ioa.close();
        }

        super.tearDown();
    }

    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void message(final Session ssn, MessageTransfer xfr)
    {
        if (queue)
        {
            messages.add(xfr);
            ssn.processed(xfr);
            return;
        }

        String body = xfr.getBodyString();

        if (body.startsWith("CLOSE"))
        {
            ssn.getConnection().close();
        }
        else if (body.startsWith("DELAYED_CLOSE"))
        {
            ssn.processed(xfr);
            new Thread()
            {
                public void run()
                {
                    try
                    {
                        sleep(3000);
                    }
                    catch (InterruptedException e)
                    {
                        throw new RuntimeException(e);
                    }
                    ssn.getConnection().close();
                }
            }.start();
        }
        else if (body.startsWith("ECHO"))
        {
            int id = xfr.getId();
            ssn.invoke(xfr);
            ssn.processed(id);
        }
        else if (body.startsWith("SINK"))
        {
            ssn.processed(xfr);
        }
        else if (body.startsWith("DROP"))
        {
            // do nothing
        }
        else if (body.startsWith("EXCP"))
        {
            ExecutionException exc = new ExecutionException();
            exc.setDescription("intentional exception for testing");
            ssn.invoke(exc);
            ssn.close();
        }
        else
        {
            throw new IllegalArgumentException
                ("unrecognized message: " + body);
        }
    }

    public void exception(Session ssn, SessionException exc)
    {
        throw exc;
    }

    public void closed(Session ssn) {}

    private void send(Session ssn, String msg)
    {
        send(ssn, msg, false);
    }

    private void send(Session ssn, String msg, boolean sync)
    {
        ssn.messageTransfer
            ("xxx", MessageAcceptMode.NONE, MessageAcquireMode.PRE_ACQUIRED,
             null, msg, sync ? SYNC : NONE);
    }

    private Connection connect(final Condition closed)
    {
        Connection conn = new Connection();
        conn.setConnectionListener(new ConnectionListener()
        {
            public void opened(Connection conn) {}
            public void exception(Connection conn, ConnectionException exc)
            {
                exc.printStackTrace();
            }
            public void closed(Connection conn)
            {
                if (closed != null)
                {
                    closed.set();
                }
            }
        });
        conn.connect("localhost", port, null, "guest", "guest", false);
        return conn;
    }

    public void testProtocolNegotiationExceptionOverridesCloseException() throws Exception
    {
        // Force os.name to be windows to exercise code in IoReceiver
        // that looks for the value of os.name
        System.setProperty("os.name","windows");

        // Start server as 0-9 to froce a ProtocolVersionException
        startServer(new ProtocolHeader(1, 0, 9));
        
        Condition closed = new Condition();

        try
        {
            connect(closed);
            fail("ProtocolVersionException expected");
        }
        catch (ProtocolVersionException pve)
        {
            //Expected code path
        }
        catch (Exception e)
        {
            fail("ProtocolVersionException expected. Got:" + e.getMessage());
        }
    }

    private void startServer()
    {
        startServer(new ProtocolHeader(1, 0, 10));
    }

    private void startServer(final ProtocolHeader protocolHeader)
    {
        ConnectionDelegate server = new ServerDelegate()
        {
            @Override
            public void init(Connection conn, ProtocolHeader hdr)
            {
                conn.send(protocolHeader);
                List<Object> utf8 = new ArrayList<Object>();
                utf8.add("utf8");
                conn.connectionStart(null, Collections.EMPTY_LIST, utf8);
            }

            @Override
            public Session getSession(Connection conn, SessionAttach atc)
            {
                Session ssn = super.getSession(conn, atc);
                ssn.setSessionListener(ConnectionTest.this);
                return ssn;
            }
        };

        try
        {
            _ioa = new IoAcceptor("localhost", port, ConnectionBinding.get(server));
        }
        catch (IOException e)
        {
            e.printStackTrace();
            fail("Unable to start Server for test due to:" + e.getMessage());
        }

        _ioa.start();
    }

    public void testClosedNotificationAndWriteToClosed() throws Exception
    {
        startServer();

        Condition closed = new Condition();
        Connection conn = connect(closed);

        Session ssn = conn.createSession(1);
        send(ssn, "CLOSE");

        if (!closed.get(3000))
        {
            fail("never got notified of connection close");
        }

        try
        {
            conn.connectionCloseOk();
            fail("writing to a closed socket succeeded");
        }
        catch (TransportException e)
        {
            // expected
        }
    }

    class FailoverConnectionListener implements ConnectionListener
    {
        public void opened(Connection conn) {}

        public void exception(Connection conn, ConnectionException e)
        {
            throw e;
        }

        public void closed(Connection conn)
        {
            queue = true;
            conn.connect("localhost", port, null, "guest", "guest");
            conn.resume();
        }
    }

    class TestSessionListener implements SessionListener
    {
        public void opened(Session s) {}
        public void resumed(Session s) {}
        public void exception(Session s, SessionException e) {}
        public void message(Session s, MessageTransfer xfr)
        {
            synchronized (incoming)
            {
                incoming.add(xfr);
                incoming.notifyAll();
            }

            s.processed(xfr);
        }
        public void closed(Session s) {}
    }

    public void testResumeNonemptyReplayBuffer() throws Exception
    {
        startServer();

        Connection conn = new Connection();
        conn.setConnectionListener(new FailoverConnectionListener());
        conn.connect("localhost", port, null, "guest", "guest");
        Session ssn = conn.createSession(1);
        ssn.setSessionListener(new TestSessionListener());

        send(ssn, "SINK 0");
        send(ssn, "ECHO 1");
        send(ssn, "ECHO 2");

        ssn.sync();

        String[] msgs = { "DROP 3", "DROP 4", "DROP 5", "CLOSE 6", "SINK 7" };
        for (String m : msgs)
        {
            send(ssn, m);
        }

        ssn.sync();

        assertEquals(msgs.length, messages.size());
        for (int i = 0; i < msgs.length; i++)
        {
            assertEquals(msgs[i], messages.get(i).getBodyString());
        }

        queue = false;

        send(ssn, "ECHO 8");
        send(ssn, "ECHO 9");

        synchronized (incoming)
        {
            Waiter w = new Waiter(incoming, 30000);
            while (w.hasTime() && incoming.size() < 4)
            {
                w.await();
            }

            assertEquals(4, incoming.size());
            assertEquals("ECHO 1", incoming.get(0).getBodyString());
            assertEquals(0, incoming.get(0).getId());
            assertEquals("ECHO 2", incoming.get(1).getBodyString());
            assertEquals(1, incoming.get(1).getId());
            assertEquals("ECHO 8", incoming.get(2).getBodyString());
            assertEquals(0, incoming.get(0).getId());
            assertEquals("ECHO 9", incoming.get(3).getBodyString());
            assertEquals(1, incoming.get(1).getId());
        }
    }

    public void testResumeEmptyReplayBuffer() throws InterruptedException
    {
        startServer();

        Connection conn = new Connection();
        conn.setConnectionListener(new FailoverConnectionListener());
        conn.connect("localhost", port, null, "guest", "guest");
        Session ssn = conn.createSession(1);
        ssn.setSessionListener(new TestSessionListener());

        send(ssn, "SINK 0");
        send(ssn, "SINK 1");
        send(ssn, "DELAYED_CLOSE 2");
        ssn.sync();
        Thread.sleep(6000);
        send(ssn, "SINK 3");
        ssn.sync();
        System.out.println(messages);
        assertEquals(1, messages.size());
        assertEquals("SINK 3", messages.get(0).getBodyString());
    }

    public void testFlushExpected() throws InterruptedException
    {
        startServer();

        Connection conn = new Connection();
        conn.connect("localhost", port, null, "guest", "guest");
        Session ssn = conn.createSession();
        ssn.sessionFlush(EXPECTED);
        send(ssn, "SINK 0");
        ssn.sessionFlush(EXPECTED);
        send(ssn, "SINK 1");
        ssn.sync();
    }

    public void testHeartbeat()
    {
        startServer();
        Connection conn = new Connection();
        conn.connect("localhost", port, null, "guest", "guest");
        conn.connectionHeartbeat();
        conn.close();
    }

    public void testExecutionExceptionInvoke() throws Exception
    {
        startServer();

        Connection conn = new Connection();
        conn.connect("localhost", port, null, "guest", "guest");
        Session ssn = conn.createSession();
        send(ssn, "EXCP 0");
        Thread.sleep(3000);
        try
        {
            send(ssn, "SINK 1");
        }
        catch (SessionException exc)
        {
            assertNotNull(exc.getException());
        }
    }

    public void testExecutionExceptionSync() throws Exception
    {
        startServer();

        Connection conn = new Connection();
        conn.connect("localhost", port, null, "guest", "guest");
        Session ssn = conn.createSession();
        send(ssn, "EXCP 0", true);
        try
        {
            ssn.sync();
        }
        catch (SessionException exc)
        {
            assertNotNull(exc.getException());
        }
    }

}
