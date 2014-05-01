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

import java.net.BindException;
import java.net.InetAddress;
import java.net.SocketAddress;

import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.ssl.SSLContextFactory;

public interface NetworkDriver extends Sender<java.nio.ByteBuffer> 
{ 
   // Creates a NetworkDriver which attempts to connect to destination on port and attaches the ProtocolEngine to  
   // it using the SSLContextFactory if provided 
   void open(int port, InetAddress destination, ProtocolEngine engine,
           NetworkDriverConfiguration config, SSLContextFactory sslFactory)
   throws OpenException; 
   
   // listens for incoming connections on the specified ports and address and creates a new NetworkDriver which 
   // processes incoming connections with ProtocolEngines and SSLEngines created from the factories 
   // (in the case of an SSLContextFactory, if provided) 
   void bind (int port, InetAddress[] addresses, ProtocolEngineFactory protocolFactory,  
              NetworkDriverConfiguration config, SSLContextFactory sslFactory) throws BindException; 
 
   // Returns the remote address of the underlying socket 
   SocketAddress getRemoteAddress();
 
   // Returns the local address of the underlying socket
   SocketAddress getLocalAddress();
   
   /**
    * The length of time after which the ProtocolEngines readIdle() method should be called if no data has been 
    * read in seconds
    */  
   void setMaxReadIdle(int idleTime);
  
   /**
    * The length of time after which the ProtocolEngines writeIdle() method should be called if no data has been 
    * written in seconds
    */   
   void setMaxWriteIdle(int idleTime);
 
} 