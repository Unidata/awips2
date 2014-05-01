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
package org.apache.qpid.client;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Hashtable;
import java.util.UUID;

import javax.jms.*;
import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NamingException;
import javax.naming.RefAddr;
import javax.naming.Reference;
import javax.naming.Referenceable;
import javax.naming.StringRefAddr;
import javax.naming.spi.ObjectFactory;

import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.url.AMQBindingURL;
import org.apache.qpid.url.URLSyntaxException;


public class AMQConnectionFactory implements ConnectionFactory, QueueConnectionFactory, TopicConnectionFactory,
                                             ObjectFactory, Referenceable, XATopicConnectionFactory,
                                             XAQueueConnectionFactory, XAConnectionFactory
{
    private String _host;
    private int _port;
    private String _defaultUsername;
    private String _defaultPassword;
    private String _virtualPath;

    private ConnectionURL _connectionDetails;
    private SSLConfiguration _sslConfig;

    public AMQConnectionFactory()
    {
    }

    /**
     * This is the Only constructor used!
     * It is used form the context and from the JNDI objects.
     */
    public AMQConnectionFactory(String url) throws URLSyntaxException
    {
        _connectionDetails = new AMQConnectionURL(url);
    }

    /**
     * This constructor is never used!
     */
    public AMQConnectionFactory(ConnectionURL url)
    {
        _connectionDetails = url;
    }

    /**
     * This constructor is never used!
     */
    public AMQConnectionFactory(String broker, String username, String password, String clientName, String virtualHost)
            throws URLSyntaxException
    {
        this(new AMQConnectionURL(
                ConnectionURL.AMQ_PROTOCOL + "://" + username + ":" + password + "@" + clientName + "/" + virtualHost + "?brokerlist='" + broker + "'"));
    }

    /**
     * This constructor is never used!
     */
    public AMQConnectionFactory(String host, int port, String virtualPath)
    {
        this(host, port, "guest", "guest", virtualPath);
    }

    /**
     * This constructor is never used!
     */
    public AMQConnectionFactory(String host, int port, String defaultUsername, String defaultPassword,
                                String virtualPath)
    {
        _host = host;
        _port = port;
        _defaultUsername = defaultUsername;
        _defaultPassword = defaultPassword;
        _virtualPath = virtualPath;

//todo when setting Host/Port has been resolved then we can use this otherwise those methods won't work with the following line.
//        _connectionDetails = new AMQConnectionURL(
//                ConnectionURL.AMQ_PROTOCOL + "://" +
//                        _defaultUsername + ":" + _defaultPassword + "@" +
//                        virtualPath + "?brokerlist='tcp://" + host + ":" + port + "'");
    }

    /**
     * @return The _defaultPassword.
     */
    public final String getDefaultPassword(String password)
    {
        if (_connectionDetails != null)
        {
            return _connectionDetails.getPassword();
        }
        else
        {
            return _defaultPassword;
        }
    }

    /**
     * @param password The _defaultPassword to set.
     */
    public final void setDefaultPassword(String password)
    {
        if (_connectionDetails != null)
        {
            _connectionDetails.setPassword(password);
        }
        _defaultPassword = password;
    }

    /**
     * Getter for SSLConfiguration
     *
     * @return SSLConfiguration if set, otherwise null
     */
    public final SSLConfiguration getSSLConfiguration()
    {
        return _sslConfig;
    }

    /**
     * Setter for SSLConfiguration
     *
     * @param sslConfig config to store
     */
    public final void setSSLConfiguration(SSLConfiguration sslConfig)
    {
        _sslConfig = sslConfig;
    }

    /**
     * @return The _defaultPassword.
     */
    public final String getDefaultUsername(String password)
    {
        if (_connectionDetails != null)
        {
            return _connectionDetails.getUsername();
        }
        else
        {
            return _defaultUsername;
        }
    }

    /**
     * @param username The _defaultUsername to set.
     */
    public final void setDefaultUsername(String username)
    {
        if (_connectionDetails != null)
        {
            _connectionDetails.setUsername(username);
        }
        _defaultUsername = username;
    }

    /**
     * @return The _host .
     */
    public final String getHost()
    {
        //todo this doesn't make sense in a multi broker URL as we have no current as that is done by AMQConnection
        return _host;
    }

    /**
     * @param host The _host to set.
     */
    public final void setHost(String host)
    {
        //todo if _connectionDetails is set then run _connectionDetails.addBrokerDetails()
        // Should perhaps have this method changed to setBroker(host,port)
        _host = host;
    }

    /**
     * @return _port The _port to set.
     */
    public final int getPort()
    {
        //todo see getHost
        return _port;
    }

    /**
     * @param port The port to set.
     */
    public final void setPort(int port)
    {
        //todo see setHost
        _port = port;
    }

    /**
     * @return he _virtualPath.
     */
    public final String getVirtualPath()
    {
        if (_connectionDetails != null)
        {
            return _connectionDetails.getVirtualHost();
        }
        else
        {
            return _virtualPath;
        }
    }

    /**
     * @param path The _virtualPath to set.
     */
    public final void setVirtualPath(String path)
    {
        if (_connectionDetails != null)
        {
            _connectionDetails.setVirtualHost(path);
        }

        _virtualPath = path;
    }

    public static String getUniqueClientID()
    {
        try
        {
            InetAddress addr = InetAddress.getLocalHost();
            return addr.getHostName() + System.currentTimeMillis();
        }
        catch (UnknownHostException e)
        {
            return "UnknownHost" + UUID.randomUUID();
        }
    }

    public Connection createConnection() throws JMSException
    {
        try
        {
            if (_connectionDetails != null)
            {
                if (_connectionDetails.getClientName() == null || _connectionDetails.getClientName().equals(""))
                {
                    _connectionDetails.setClientName(getUniqueClientID());
                }
                return new AMQConnection(_connectionDetails, _sslConfig);
            }
            else
            {
                return new AMQConnection(_host, _port, _defaultUsername, _defaultPassword, getUniqueClientID(),
                                         _virtualPath);
            }
        }
        catch (Exception e)
        {
            JMSException jmse = new JMSException("Error creating connection: " + e.getMessage());
            jmse.setLinkedException(e);
            throw jmse;
        }


    }

    public Connection createConnection(String userName, String password) throws JMSException
    {
        return createConnection(userName, password, null);
    }
    
    public Connection createConnection(String userName, String password, String id) throws JMSException
    {
        try
        {
            if (_connectionDetails != null)
            {
                _connectionDetails.setUsername(userName);
                _connectionDetails.setPassword(password);
                
                if (id != null && !id.equals(""))
                {
                    _connectionDetails.setClientName(id);
                } 
                else if (_connectionDetails.getClientName() == null || _connectionDetails.getClientName().equals(""))
                {
                    _connectionDetails.setClientName(getUniqueClientID());
                }
                return new AMQConnection(_connectionDetails, _sslConfig);
            }
            else
            {
                return new AMQConnection(_host, _port, userName, password, (id != null ? id : getUniqueClientID()), _virtualPath);
            }
        }
        catch (Exception e)
        {
            JMSException jmse = new JMSException("Error creating connection: " + e.getMessage());
            jmse.setLinkedException(e);
            throw jmse;
        }
    }

    public QueueConnection createQueueConnection() throws JMSException
    {
        return (QueueConnection) createConnection();
    }

    public QueueConnection createQueueConnection(String username, String password) throws JMSException
    {
        return (QueueConnection) createConnection(username, password);
    }

    public TopicConnection createTopicConnection() throws JMSException
    {
        return (TopicConnection) createConnection();
    }

    public TopicConnection createTopicConnection(String username, String password) throws JMSException
    {
        return (TopicConnection) createConnection(username, password);
    }


    public ConnectionURL getConnectionURL()
    {
        return _connectionDetails;
    }

    public String getConnectionURLString()
    {
        return _connectionDetails.toString();
    }


    public final void setConnectionURLString(String url) throws URLSyntaxException
    {
        _connectionDetails = new AMQConnectionURL(url);
    }

    /**
     * JNDI interface to create objects from References.
     *
     * @param obj  The Reference from JNDI
     * @param name
     * @param ctx
     * @param env
     *
     * @return AMQConnection,AMQTopic,AMQQueue, or AMQConnectionFactory.
     *
     * @throws Exception
     */
    public Object getObjectInstance(Object obj, Name name, Context ctx, Hashtable env) throws Exception
    {
        if (obj instanceof Reference)
        {
            Reference ref = (Reference) obj;

            if (ref.getClassName().equals(AMQConnection.class.getName()))
            {
                RefAddr addr = ref.get(AMQConnection.class.getName());

                if (addr != null)
                {
                    return new AMQConnection((String) addr.getContent());
                }
            }

            if (ref.getClassName().equals(AMQQueue.class.getName()))
            {
                RefAddr addr = ref.get(AMQQueue.class.getName());

                if (addr != null)
                {
                    return new AMQQueue(new AMQBindingURL((String) addr.getContent()));
                }
            }

            if (ref.getClassName().equals(AMQTopic.class.getName()))
            {
                RefAddr addr = ref.get(AMQTopic.class.getName());

                if (addr != null)
                {
                    return new AMQTopic(new AMQBindingURL((String) addr.getContent()));
                }
            }

            if (ref.getClassName().equals(AMQConnectionFactory.class.getName()))
            {
                RefAddr addr = ref.get(AMQConnectionFactory.class.getName());

                if (addr != null)
                {
                    return new AMQConnectionFactory((String) addr.getContent());
                }
            }

        }
        return null;
    }


    public Reference getReference() throws NamingException
    {
        return new Reference(
                AMQConnectionFactory.class.getName(),
                new StringRefAddr(AMQConnectionFactory.class.getName(), _connectionDetails.getURL()),
                             AMQConnectionFactory.class.getName(), null);          // factory location
    }

    // ---------------------------------------------------------------------------------------------------
    // the following methods are provided for XA compatibility
    // Those methods are only supported by 0_10 and above 
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a XAConnection with the default user identity.
     * <p> The XAConnection is created in stopped mode. No messages
     * will be delivered until the <code>Connection.start</code> method
     * is explicitly called.
     *
     * @return A newly created XAConnection
     * @throws JMSException         If creating the XAConnection fails due to some internal error.
     * @throws JMSSecurityException If client authentication fails due to an invalid user name or password.
     */
    public XAConnection createXAConnection() throws JMSException
    {
        try
        {
            return new XAConnectionImpl(_connectionDetails, _sslConfig);
        }
        catch (Exception e)
        {
            JMSException jmse = new JMSException("Error creating connection: " + e.getMessage());
            jmse.setLinkedException(e);
            throw jmse;
        }
    }

    /**
     * Creates a XAConnection with the specified user identity.
     * <p> The XAConnection is created in stopped mode. No messages
     * will be delivered until the <code>Connection.start</code> method
     * is explicitly called.
     *
     * @param username the caller's user name
     * @param password the caller's password
     * @return A newly created XAConnection.
     * @throws JMSException         If creating the XAConnection fails due to some internal error.
     * @throws JMSSecurityException If client authentication fails due to an invalid user name or password.
     */
    public XAConnection createXAConnection(String username, String password) throws JMSException
    {
        if (_connectionDetails != null)
        {
            _connectionDetails.setUsername(username);
            _connectionDetails.setPassword(password);

            if (_connectionDetails.getClientName() == null || _connectionDetails.getClientName().equals(""))
            {
                _connectionDetails.setClientName(getUniqueClientID());
            }
        }
        else
        {
            throw new JMSException("A URL must be specified to access XA connections");
        }
        return createXAConnection();
    }


    /**
     * Creates a XATopicConnection with the default user identity.
     * <p> The XATopicConnection is created in stopped mode. No messages
     * will be delivered until the <code>Connection.start</code> method
     * is explicitly called.
     *
     * @return A newly created XATopicConnection
     * @throws JMSException         If creating the XATopicConnection fails due to some internal error.
     * @throws JMSSecurityException If client authentication fails due to an invalid user name or password.
     */
    public XATopicConnection createXATopicConnection() throws JMSException
    {
        return (XATopicConnection) createXAConnection();
    }

    /**
     * Creates a XATopicConnection with the specified user identity.
     * <p> The XATopicConnection is created in stopped mode. No messages
     * will be delivered until the <code>Connection.start</code> method
     * is explicitly called.
     *
     * @param username the caller's user name
     * @param password the caller's password
     * @return A newly created XATopicConnection.
     * @throws JMSException         If creating the XATopicConnection fails due to some internal error.
     * @throws JMSSecurityException If client authentication fails due to an invalid user name or password.
     */
    public XATopicConnection createXATopicConnection(String username, String password) throws JMSException
    {
         return (XATopicConnection) createXAConnection(username, password);
    }

    /**
     * Creates a XAQueueConnection with the default user identity.
     * <p> The XAQueueConnection is created in stopped mode. No messages
     * will be delivered until the <code>Connection.start</code> method
     * is explicitly called.
     *
     * @return A newly created XAQueueConnection
     * @throws JMSException         If creating the XAQueueConnection fails due to some internal error.
     * @throws JMSSecurityException If client authentication fails due to an invalid user name or password.
     */
    public XAQueueConnection createXAQueueConnection() throws JMSException
    {
       return (XAQueueConnection) createXAConnection();
    }

    /**
     * Creates a XAQueueConnection with the specified user identity.
     * <p> The XAQueueConnection is created in stopped mode. No messages
     * will be delivered until the <code>Connection.start</code> method
     * is explicitly called.
     *
     * @param username the caller's user name
     * @param password the caller's password
     * @return A newly created XAQueueConnection.
     * @throws JMSException         If creating the XAQueueConnection fails due to some internal error.
     * @throws JMSSecurityException If client authentication fails due to an invalid user name or password.
     */
    public XAQueueConnection createXAQueueConnection(String username, String password) throws JMSException
    {
        return (XAQueueConnection) createXAConnection(username, password);
    }
}
