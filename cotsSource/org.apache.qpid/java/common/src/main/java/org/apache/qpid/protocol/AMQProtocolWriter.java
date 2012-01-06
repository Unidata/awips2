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

import org.apache.qpid.framing.AMQDataBlock;

/**
 * AMQProtocolWriter provides a method to write a frame of data 'to the wire', in the context of the object
 * that implements the method, usually some sort of session. The block of data, encapsulated by {@link AMQDataBlock},
 * will be encoded as it is written.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Write an encoded block of data to the write, in the context of a session.
 * </table>
 */
public interface AMQProtocolWriter
{
    /**
     * Writes a frame to the wire, encoding it as necessary, for example, into a sequence of bytes.
     *
     * @param frame The frame to be encoded and written.
     */
    public void writeFrame(AMQDataBlock frame);
}
