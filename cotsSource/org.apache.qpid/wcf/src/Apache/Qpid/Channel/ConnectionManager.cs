/*
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
*/

namespace Apache.Qpid.Channel
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Threading;

    using Apache.Qpid.Interop;

    // The ConnectionManager looks after a shareable pool of AmqpConnection and AmqpSession
    // objects.  If two connection requests could be shared (see MakeKey() properties), and
    // are designated as shareable, then they will be paired up.  Each shared connection is
    // a separate instance of a ManagedConnection.  All unshared connections use a single
    // instance of ManagedConnection with locking turned off.  The ManagedConnection object
    // registers for notifictation when a connection goes idle (all grandchild InputLink and 
    // OutputLink objects have been closed), and closes the connection.

    // TODO: the session sharing is roughed-in via comments but needs completing.

    internal sealed class ConnectionManager
    {
        // A side effect of creating InputLinks and OutputLinks is that counters 
        // in the respective AmqpSession and AmqpConnection are updated, so care
        // must be taken to hold the lock across acquiring a session and opening
        // a link on it.

        // one for each shared connection
        private static Dictionary<string, ManagedConnection> sharedInstances;

        // this one creates and releases connections that are not shared.  No locking required.
        private static ManagedConnection unsharedInstance;

        // lock for finding or creating ManagedConnection instances 
        private static Object connectionLock;

        static ConnectionManager()
        {
            unsharedInstance = null;
            sharedInstances = new Dictionary<string, ManagedConnection>();
            connectionLock = new Object();
        }

        private static string MakeKey(AmqpChannelProperties props)
        {
            return props.BrokerHost + ':' + props.BrokerPort + ':' + props.TransferMode;
        }

        private static ManagedConnection GetManagedConnection(AmqpChannelProperties channelProperties, bool connectionSharing)
        {
            if (connectionSharing)
            {
                string key = MakeKey(channelProperties);
                lock (connectionLock)
                {
                    ManagedConnection mc = null;
                    if (!sharedInstances.TryGetValue(key, out mc))
                    {
                        mc = new ManagedConnection(true);
                        sharedInstances.Add(key, mc);
                    }
                    return mc;
                }
            }
            else
            {
                lock (connectionLock)
                {
                    if (unsharedInstance == null)
                    {
                        unsharedInstance = new ManagedConnection(false);
                    }
                    return unsharedInstance;
                }
            }
        }

        public static OutputLink GetOutputLink(AmqpChannelProperties channelProperties, bool connectionSharing, bool sessionSharing, string qname)
        {
            ManagedConnection mc = GetManagedConnection(channelProperties, connectionSharing);
            return (OutputLink)mc.GetLink(channelProperties, sessionSharing, null, qname);
        }

        public static InputLink GetInputLink(AmqpChannelProperties channelProperties, bool connectionSharing, bool sessionSharing, string qname)
        {
            ManagedConnection mc = GetManagedConnection(channelProperties, connectionSharing);
            return (InputLink)mc.GetLink(channelProperties, sessionSharing, qname, null);
        }



        class ManagedConnection
        {
            private Boolean shared;
            private AmqpConnection sharedConnection;
            //private Dictionary<string, AmqpSession> sharedSessions;

            public ManagedConnection(bool shared)
            {
                this.shared = shared;
            }


            public object GetLink(AmqpChannelProperties channelProperties, bool sessionSharing, string inputQueue, string outputQueue)
            {
                AmqpConnection connection = null;
                AmqpSession session = null;
                Object link = null;
                bool newConnection = false;
                //bool newSession = false;
                bool success = false;

                // when called in the non-shared case, only stack variables should be used for holding connections/sessions/links

                if (this.shared)
                {
                    Monitor.Enter(this); // lock
                }

                try
                {
                    if (this.shared)
                    {
                        // TODO: check shared connection not closed (i.e. network drop) and refresh this instance if needed
                        if (sessionSharing)
                        {
                            throw new NotImplementedException("shared session");
                            /* * ... once we have a defined shared session config parameter:

                            // lazilly create
                            if (this.sharedSessions == null)
                            {
                                this.sharedSessions = new Dictionary<string, AmqpSession>();
                            }

                            alreadydeclaredstring sessionKey = channelProperties.name_of_key_goes_here;
                            this.sharedSessions.TryGetValue(sessionKey, out session);
                       
                            * */
                        }

                        if (this.sharedConnection != null)
                        {
                            connection = this.sharedConnection;
                        }
                    }

                    if (connection == null)
                    {
                        connection = new AmqpConnection(channelProperties.BrokerHost, channelProperties.BrokerPort);
                        newConnection = true;
                        if (this.shared)
                        {
                            connection.OnConnectionIdle += new ConnectionIdleEventHandler(this.IdleConnectionHandler);
                        }
                        else
                        {
                            connection.OnConnectionIdle += new ConnectionIdleEventHandler(UnsharedIdleConnectionHandler);
                        }
                    }

                    if (session == null)
                    {
                        session = connection.CreateSession();
                        //newSession = true;
                    }

                    if (inputQueue != null)
                    {
                        link = session.CreateInputLink(inputQueue);
                    }
                    else
                    {
                        link = session.CreateOutputLink(outputQueue);
                    }

                    if (this.shared)
                    {
                        if (newConnection)
                        {
                            this.sharedConnection = connection;
                        }
                        /*
                        if (newSession)
                        {
                            sharedSessions.Add(foo, session);
                        }
                         * */
                    }

                    success = true;
                }
                finally
                {
                    if (this.shared)
                    {
                        Monitor.Exit(this);
                    }
                    if (!success)
                    {
                        /*
                        if (newSession)
                        {
                            session.Close();
                        }
                         */
                        if (newConnection)
                        {
                            connection.Close();
                        }
                    }
                }

                return link;
            }


            static void UnsharedIdleConnectionHandler(Object sender, EventArgs empty)
            {
                if (sender is AmqpConnection)
                {
                    AmqpConnection connection = (AmqpConnection)sender;
                    connection.Close();
                }
            }

            void IdleConnectionHandler(Object sender, EventArgs empty)
            {
                lock (this)
                {
                    if (sharedConnection != sender || sharedConnection == null)
                    {
                        return;
                    }
                    if (!sharedConnection.IsIdle)
                    {
                        // Another thread made the connection busy again.
                        // That's OK.  Another idle event will come along later.
                        return;
                    }
                    sharedConnection.Close();  // also closes all child sessions
                    sharedConnection = null;
                    //sharedSessions = null;
                }
            }
        }
    }
}
