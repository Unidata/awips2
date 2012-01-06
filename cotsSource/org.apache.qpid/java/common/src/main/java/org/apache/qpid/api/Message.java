package org.apache.qpid.api;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.apache.qpid.transport.MessageProperties;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.Header;

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

public interface Message
{
    public Header getHeader();

    public void setHeader(Header header);

    public MessageProperties getMessageProperties();

	public DeliveryProperties getDeliveryProperties();

	/**
	 * This will abstract the underlying message data.
	 * The Message implementation may not hold all message
	 * data in memory (especially in the case of large messages)
	 *
	 * The appendData function might write data to
	 * <ul>
	 * <li> Memory (Ex: ByteBuffer)
	 * <li> To Disk
	 * <li> To Socket (Stream)
	 * </ul>
	 * @param src - the data to append
	 */
	public void appendData(byte[] src) throws IOException;


    /**
     * This will abstract the underlying message data.
     * The Message implementation may not hold all message
     * data in memory (especially in the case of large messages)
     *
     * The appendData function might write data to
     * <ul>
     * <li> Memory (Ex: ByteBuffer)
     * <li> To Disk
     * <li> To Socket (Stream)
     * </ul>
     * @param src - the data to append
     */
    public void appendData(ByteBuffer src) throws IOException;

	/**
	 * This will abstract the underlying message data.
	 * The Message implementation may not hold all message
	 * data in memory (especially in the case of large messages)
	 *
	 * The read function might copy data from
	 * <ul>
	 * <li> From memory (Ex: ByteBuffer)
	 * <li> From Disk
	 * <li> From Socket as and when it gets streamed
	 * </ul>
	 * @param target  The target byte[] which the data gets copied to
	 */
    public void readData(byte[] target) throws IOException;

    /**
     * * This will abstract the underlying message data.
     * The Message implementation may not hold all message
     * data in memory (especially in the case of large messages)
     *
     * The read function might copy data from
     * <ul>
     * <li> From memory (Ex: ByteBuffer)
     * <li> From Disk
     * <li> From Socket as and when it gets streamed
     * </ul>
     *
     * @return A ByteBuffer containing data
     * @throws IOException
     */
    public ByteBuffer readData() throws IOException;

    /**
     * This should clear the body of the message.
     */
    public void clearData();

    /**
     * The provides access to the command Id assigned to the
     * message transfer.
     * This id is useful when you do
     * <ul>
     * <li>For message acquiring - If the transfer happend in no-acquire mode
     *     you could use this id to accquire it.
     * <li>For releasing a message. You can use this id to release an acquired
     *     message
     * <li>For Acknowledging a message - You need to pass this ID, in order to
     *     acknowledge the message
     * <li>For Rejecting a message - You need to pass this ID, in order to reject
     *     the message.
     * </ul>
     *
     * @return the message transfer id.
     */
    public int getMessageTransferId();

}
