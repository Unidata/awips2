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

import org.apache.qpid.transport.network.ConnectionBinding;
import org.apache.qpid.transport.network.io.IoTransport;
import org.apache.qpid.transport.util.Logger;
import org.apache.qpid.transport.util.Waiter;
import org.apache.qpid.util.Strings;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.UUID;

import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslServer;

import static org.apache.qpid.transport.Connection.State.*;


/**
 * Connection
 *
 * @author Rafael H. Schloming
 *
 * @todo the channels map should probably be replaced with something
 * more efficient, e.g. an array or a map implementation that can use
 * short instead of Short
 */

public class Connection extends ConnectionInvoker
    implements Receiver<ProtocolEvent>, Sender<ProtocolEvent>
{

    private static final Logger log = Logger.get(Connection.class);

    public enum State { NEW, CLOSED, OPENING, OPEN, CLOSING, CLOSE_RCVD }

    class DefaultConnectionListener implements ConnectionListener
    {
        public void opened(Connection conn) {}
        public void exception(Connection conn, ConnectionException exception)
        {
            log.error(exception, "connection exception");
        }
        public void closed(Connection conn) {}
    }

    private ConnectionDelegate delegate;
    private Sender<ProtocolEvent> sender;

    final private Map<Binary,Session> sessions = new HashMap<Binary,Session>();
    final private Map<Integer,Session> channels = new HashMap<Integer,Session>();

    private State state = NEW;
    final private Object lock = new Object();
    private long timeout = 60000;
    private ConnectionListener listener = new DefaultConnectionListener();
    private ConnectionException error = null;

    private int channelMax = 1;
    private String locale;
    private SaslServer saslServer;
    private SaslClient saslClient;
    private long idleTimeout = 0;
    private String _authorizationID;

    // want to make this final
    private int _connectionId;

    public Connection() {}

    public void setConnectionDelegate(ConnectionDelegate delegate)
    {
        this.delegate = delegate;
    }

    public void setConnectionListener(ConnectionListener listener)
    {
        if (listener == null)
        {
            this.listener = new DefaultConnectionListener();
        }
        else
        {
            this.listener = listener;
        }
    }

    public Sender<ProtocolEvent> getSender()
    {
        return sender;
    }

    public void setSender(Sender<ProtocolEvent> sender)
    {
        this.sender = sender;
        sender.setIdleTimeout(idleTimeout);         
    }

    protected void setState(State state)
    {
        synchronized (lock)
        {
            this.state = state;
            lock.notifyAll();
        }
    }

    void setLocale(String locale)
    {
        this.locale = locale;
    }

    String getLocale()
    {
        return locale;
    }

    void setSaslServer(SaslServer saslServer)
    {
        this.saslServer = saslServer;
    }

    SaslServer getSaslServer()
    {
        return saslServer;
    }

    void setSaslClient(SaslClient saslClient)
    {
        this.saslClient = saslClient;
    }

    SaslClient getSaslClient()
    {
        return saslClient;
    }

    public void connect(String host, int port, String vhost, String username, String password)
    {
        connect(host, port, vhost, username, password, false);
    }
    
    public void connect(String host, int port, String vhost, String username, String password, boolean ssl)
    {
        connect(host, port, vhost, username, password, ssl,"PLAIN");
    }

    public void connect(String host, int port, String vhost, String username, String password, boolean ssl,String saslMechs)
    {
        synchronized (lock)
        {
            state = OPENING;

            delegate = new ClientDelegate(vhost, username, password,saslMechs);

            IoTransport.connect(host, port, ConnectionBinding.get(this), ssl);
            send(new ProtocolHeader(1, 0, 10));

            Waiter w = new Waiter(lock, timeout);
            while (w.hasTime() && state == OPENING && error == null)
            {
                w.await();
            }

            if (error != null)
            {
                ConnectionException t = error;
                error = null;
                try
                {
                    close();
                }
                catch (ConnectionException ce)
                {
                    if (!(t instanceof ProtocolVersionException))
                    {
                        throw ce;
                    }
                }
                t.rethrow();
            }

            switch (state)
            {
            case OPENING:
                close();
                throw new ConnectionException("connect() timed out");
            case OPEN:
                break;
            case CLOSED:
                throw new ConnectionException("connect() aborted");
            default:
                throw new IllegalStateException(String.valueOf(state));
            }
        }

        listener.opened(this);
    }

    public Session createSession()
    {
        return createSession(0);
    }

    public Session createSession(long expiry)
    {
        return createSession(UUID.randomUUID().toString(), expiry);
    }

    public Session createSession(String name)
    {
        return createSession(name, 0);
    }

    public Session createSession(String name, long expiry)
    {
        return createSession(Strings.toUTF8(name), expiry);
    }

    public Session createSession(byte[] name, long expiry)
    {
        return createSession(new Binary(name), expiry);
    }

    public Session createSession(Binary name, long expiry)
    {
        synchronized (lock)
        {
            Session ssn = new Session(this, name, expiry);
            sessions.put(name, ssn);
            map(ssn);
            ssn.attach();
            return ssn;
        }
    }

    void removeSession(Session ssn)
    {
        synchronized (lock)
        {
            sessions.remove(ssn.getName());
        }
    }

    public void setConnectionId(int id)
    {
        _connectionId = id;
    }

    public int getConnectionId()
    {
        return _connectionId;
    }

    public ConnectionDelegate getConnectionDelegate()
    {
        return delegate;
    }

    public void received(ProtocolEvent event)
    {
        log.debug("RECV: [%s] %s", this, event);
        event.delegate(this, delegate);
    }

    public void send(ProtocolEvent event)
    {
        log.debug("SEND: [%s] %s", this, event);
        Sender s = sender;
        if (s == null)
        {
            throw new ConnectionException("connection closed");
        }
        s.send(event);
    }

    public void flush()
    {
        log.debug("FLUSH: [%s]", this);
        sender.flush();
    }

    protected void invoke(Method method)
    {
        method.setChannel(0);
        send(method);
        if (!method.isBatch())
        {
            flush();
        }
    }

    public void dispatch(Method method)
    {
        Session ssn = getSession(method.getChannel());
        if(ssn != null)
        {
            ssn.received(method);
        }
        else
        {
            throw new ProtocolViolationException(
					"Received frames for an already dettached session", null);
        }
    }

    public int getChannelMax()
    {
        return channelMax;
    }

    void setChannelMax(int max)
    {
        channelMax = max;
    }

    private int map(Session ssn)
    {
        synchronized (lock)
        {
            for (int i = 0; i < getChannelMax(); i++)
            {
                if (!channels.containsKey(i))
                {
                    map(ssn, i);
                    return i;
                }
            }

            throw new RuntimeException("no more channels available");
        }
    }

    void map(Session ssn, int channel)
    {
        synchronized (lock)
        {
            channels.put(channel, ssn);
            ssn.setChannel(channel);
        }
    }

    void unmap(Session ssn)
    {
        synchronized (lock)
        {
            channels.remove(ssn.getChannel());
        }
    }

    Session getSession(int channel)
    {
        synchronized (lock)
        {
            return channels.get(channel);
        }
    }

    public void resume()
    {
        synchronized (lock)
        {
            for (Session ssn : sessions.values())
            {
                map(ssn);
                ssn.attach();
                ssn.resume();
            }
        }
    }

    public void exception(ConnectionException e)
    {
        synchronized (lock)
        {
            switch (state)
            {
            case OPENING:
            case CLOSING:
                error = e;
                lock.notifyAll();
                return;
            }
        }

        listener.exception(this, e);
    }

    public void exception(Throwable t)
    {
        exception(new ConnectionException(t));
    }

    void closeCode(ConnectionClose close)
    {
        synchronized (lock)
        {
            for (Session ssn : channels.values())
            {
                ssn.closeCode(close);
            }
            ConnectionCloseCode code = close.getReplyCode();
            if (code != ConnectionCloseCode.NORMAL)
            {
                exception(new ConnectionException(close));
            }
        }
    }

    public void closed()
    {
        if (state == OPEN)
        {
            exception(new ConnectionException("connection aborted"));
        }

        log.debug("connection closed: %s", this);

        synchronized (lock)
        {
            List<Session> values = new ArrayList<Session>(channels.values());
            for (Session ssn : values)
            {
                ssn.closed();
            }

            try
            {
                sender.close();
            }
            catch(Exception e)
            {
                // ignore.
            }
            sender = null;
            setState(CLOSED);
        }

        listener.closed(this);
    }

    public void close()
    {
        synchronized (lock)
        {
            switch (state)
            {
            case OPEN:
                state = CLOSING;
                connectionClose(ConnectionCloseCode.NORMAL, null);
                Waiter w = new Waiter(lock, timeout);
                while (w.hasTime() && state == CLOSING && error == null)
                {
                    w.await();
                }

                if (error != null)
                {
                    close();
                    throw new ConnectionException(error);
                }

                switch (state)
                {
                case CLOSING:
                    close();
                    throw new ConnectionException("close() timed out");
                case CLOSED:
                    break;
                default:
                    throw new IllegalStateException(String.valueOf(state));
                }
                break;
            case CLOSED:
                break;
            default:
                if (sender != null)
                {
                    sender.close();
                    w = new Waiter(lock, timeout);
                    while (w.hasTime() && sender != null && error == null)
                    {
                        w.await();
                    }

                    if (error != null)
                    {
                        throw new ConnectionException(error);
                    }

                    if (sender != null)
                    {
                        throw new ConnectionException("close() timed out");
                    }
                }
                break;
            }
        }
    }

    public void setIdleTimeout(long l)
    {
        idleTimeout = l;       
        if (sender != null)
        {            
            sender.setIdleTimeout(l);    
        }
    }
    
    public long getIdleTimeout()
    {
        return idleTimeout;
    }

    public void setAuthorizationID(String authorizationID)
    {
        _authorizationID = authorizationID;
    }

    public String getAuthorizationID()
    {
        return _authorizationID;
    }

    public String toString()
    {
        return String.format("conn:%x", System.identityHashCode(this));
    }

}
