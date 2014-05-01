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

/**
 * This interface provides a means for NetworkDrivers to configure TCP options such as incoming and outgoing
 * buffer sizes and set particular options on the socket. NetworkDrivers should honour the values returned
 * from here if the underlying implementation supports them.  
 */ 
public interface NetworkDriverConfiguration  
{  
    // Taken from Socket  
    Boolean getKeepAlive();
    Boolean getOOBInline();
    Boolean getReuseAddress();
    Integer getSoLinger(); // null means off 
    Integer getSoTimeout(); 
    Boolean getTcpNoDelay(); 
    Integer getTrafficClass();

    // The amount of memory in bytes to allocate to the incoming buffer 
    Integer getReceiveBufferSize();  

    // The amount of memory in bytes to allocate to the outgoing buffer 
    Integer getSendBufferSize();  
} 
