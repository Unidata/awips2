/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.example.pubsub;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;
import javax.naming.NamingException;

/**
 * An abstract base class that wraps up the creation of a JMS client utilising JNDI
 */
public abstract class Client
{
    protected ConnectionSetup _setup;

    protected Connection _connection;
    protected Destination _destination;
    protected Session _session;

    public Client(String destination)
    {
        if (destination == null)
        {
            destination = ConnectionSetup.TOPIC_JNDI_NAME;
        }

        try
        {
            _setup = new ConnectionSetup();
        }
        catch (NamingException e)
        {
            //ignore
        }

        if (_setup != null)
        {
            try
            {
                _connection = _setup.getConnectionFactory().createConnection();
                _destination = _setup.getDestination(destination);
            }
            catch (JMSException e)
            {
                System.err.println(e.getMessage());
            }
        }
    }

    public abstract void start();

}