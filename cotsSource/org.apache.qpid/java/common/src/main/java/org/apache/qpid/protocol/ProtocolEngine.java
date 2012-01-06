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
package org.apache.qpid.protocol;

import java.net.SocketAddress;

import org.apache.qpid.framing.AMQDataBlock;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.transport.Receiver;

/**
 * A ProtocolEngine is a Receiver for java.nio.ByteBuffers. It takes the data passed to it in the received
 * decodes it and then process the result.
 */
public interface ProtocolEngine extends Receiver<java.nio.ByteBuffer>
{
   // Sets the network driver providing data for this ProtocolEngine
   void setNetworkDriver (NetworkDriver driver);

   // Returns the remote address of the NetworkDriver
   SocketAddress getRemoteAddress();

   // Returns the local address of the NetworkDriver
   SocketAddress getLocalAddress();

   // Returns number of bytes written
   long getWrittenBytes();

   // Returns number of bytes read
   long getReadBytes();

   // Called by the NetworkDriver when the socket has been closed for reading
   void closed();

   // Called when the NetworkEngine has not written data for the specified period of time (will trigger a
   // heartbeat)
   void writerIdle();

   // Called when the NetworkEngine has not read data for the specified period of time (will close the connection)
   void readerIdle();


}