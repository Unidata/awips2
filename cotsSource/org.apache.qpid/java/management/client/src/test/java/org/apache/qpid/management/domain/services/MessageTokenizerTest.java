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

package org.apache.qpid.management.domain.services;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.*;

import junit.framework.TestCase;

import org.apache.qpid.api.Message;
import org.apache.qpid.nclient.util.ByteBufferMessage;
import org.apache.qpid.transport.codec.BBDecoder;

/**
 * Tests case for messaeg tokenizer.
 * 
 * @author Andrea Gazzarini
 */
public class MessageTokenizerTest extends TestCase {
	
	/**
	 * Tests the execution of the message tokenizer when the given message is not a valid AMQP message.
	 * 
	 * <br>precondition : the incoming message is not a valid AMQP message.
	 * <br>postcondition : no exception is thrown and there will be exactly one token with the given message.
	 */
	public void testOK_WithNoMessage() throws IOException{
		byte [] noMessage = {2,10,120,23,23,23,4,10,11,12,2,1,3,-22};
		
		Message multiMessage = new ByteBufferMessage();
		multiMessage.appendData(noMessage);
		MessageTokenizer tokenizer = new MessageTokenizer(multiMessage);
		
		assertEquals(1, tokenizer.countTokens());
		assertEquals(tokenizer.nextElement(),noMessage);
		assertFalse(tokenizer.hasMoreElements());
	}
	
	/**
	 * Tests the execution of the message tokenizer when the given message contains only one message.
	 * 
	 * <br>precondition : the incoming message contains only one message.
	 * <br>postcondition : no exception is thrown and there will be exactly one token with the given message.
	 */
	public void testOK_WithOneMessage() throws IOException{
		byte [] oneEncodedMessage = {'A','M','2',23,23,23,4,10,11,12,2,1,3,-22};
		
		Message multiMessage = new ByteBufferMessage();
		multiMessage.appendData(oneEncodedMessage);
		MessageTokenizer tokenizer = new MessageTokenizer(multiMessage);
		
		assertEquals(1, tokenizer.countTokens());
		assertEquals(tokenizer.nextElement(),oneEncodedMessage);
		assertFalse(tokenizer.hasMoreElements());
	}

	/**
	 * Tests the execution of the message tokenizer when the given message contains a random number of messages.
	 * 
	 * <br>precondition : the incoming message contains a random number of messages.
	 * <br>postcondition : no exception is thrown and each built token is a valid message starting with right header.
	 */
	public void testOK_WithRandomNUmberOfMessages() throws IOException{
		Random randomizer = new Random();
		
		int howManyLoops = randomizer.nextInt(10000);
		howManyLoops = (howManyLoops == 0) ? 10 : howManyLoops;
		byte [] compoundMessageData = new byte [12 * howManyLoops];
		
		List<byte []> messages = new ArrayList<byte[]>(howManyLoops);
		
		int position = 0;
		for (int i = 0; i < howManyLoops; i++) 
		{
			byte [] message = new byte[12];			
			System.arraycopy(MessageTokenizer.MAGIC_NUMBER_BYTES, 0, compoundMessageData, position, MessageTokenizer.MAGIC_NUMBER_BYTES.length);
			System.arraycopy(MessageTokenizer.MAGIC_NUMBER_BYTES, 0, message, 0, MessageTokenizer.MAGIC_NUMBER_BYTES.length);
			position+=MessageTokenizer.MAGIC_NUMBER_BYTES.length;
			
			for (int c = 3; c < 12; c++) 
			{
				byte aByte = (byte)randomizer.nextInt(127);
				aByte = (aByte == 77) ? (byte)c : aByte;
				compoundMessageData[position++] = aByte;
				message[c] = aByte;
			}
			messages.add(message);
		}
				
		Message multiMessage = new ByteBufferMessage();
		multiMessage.appendData(compoundMessageData);
		MessageTokenizer tokenizer = new MessageTokenizer(multiMessage);
		
		int howManyTokens = tokenizer.countTokens();
		assertEquals(howManyLoops, howManyTokens);
		
		int index = 0;
		while (tokenizer.hasMoreElements())
		{
			assertEquals(tokenizer.nextElement(),messages.get(index++));
		}	
		
		assertEquals((index),howManyTokens);
	}
	
	/**
	 * Internal method used for comparison of two messages.
	 * 
	 * @param message the token message just built by the tokenizer.
	 * @param expected the expected result.
	 */
	private void assertEquals(Message message, byte [] expected) throws IOException 
	{
		ByteBuffer messageContent = message.readData();
		BBDecoder decoder = new BBDecoder();
		decoder.init(messageContent);
		byte [] content = decoder.readReaminingBytes();	
		assertTrue(Arrays.equals(content, expected));		
	}
}
