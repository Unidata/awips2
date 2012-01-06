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
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;

import org.apache.qpid.api.Message;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.nclient.util.ByteBufferMessage;
import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.util.Logger;

/**
 * The message tokenizer class allows a multi message listener to break a 
 * message into tokens where each token is itself a valid AMQP message.
 * 
 * @author  Andrea Gazzarini
 * @see QPID-1368
 */
class MessageTokenizer implements Enumeration<Message> 
{
	private final static Logger LOGGER = Logger.get(MessageTokenizer.class);
	
	static byte [] MAGIC_NUMBER_BYTES;
	
	private LinkedList<Message> _messages = new LinkedList<Message>();
	private Iterator<Message> _iterator;
	
	static 
	{
		try 
		{
			MAGIC_NUMBER_BYTES = Protocol.MAGIC_NUMBER.getBytes("UTF-8");
		} catch(Exception exception) 
		{
			throw new ExceptionInInitializerError(exception);
		}
	}
	
	/**
	 * Builds a new Message tokenizer with the given message.
	 * Note that if the given message is not a "compound" message this tokenizer will producer only one token; 
	 * That is, the token is a message equals to the given message.
	 * 
	 * @param compoundMessage the compound message
	 * @throws IOException when it's not possible to read the given message content.
	 */
	MessageTokenizer(Message compoundMessage) throws IOException
	{
		build(compoundMessage);
	}
	
	public boolean hasMoreElements() 
	{
		return _iterator.hasNext();
	}

	public Message nextElement() 
	{
		return _iterator.next();
	}

	/**
	 * Retruns the number of the tokens produced by this tokenizer.
	 * 
	 * @return the number of the tokens produced by this tokenizer.
	 */
	public int countTokens() 
	{
		return _messages.size();
	}

	// Internal methods used for splitting the multi message byte array.
	int indexOf(byte[] source, int startIndex) 
	{
		int currentSourceIndex; 
		int currentExampleIndex;
		
		if (startIndex + 3 > source.length)
			return -1;

		for (currentSourceIndex = startIndex; currentSourceIndex <= source.length - 3; currentSourceIndex++) 
		{
			for (currentExampleIndex = 0; currentExampleIndex < 3; currentExampleIndex++) 
			{
				if (source[currentSourceIndex + currentExampleIndex] != MAGIC_NUMBER_BYTES[currentExampleIndex])
					break;
			}
			
			if (currentExampleIndex == 3)
				return currentSourceIndex;
		}
		return -1;
	}	
	
	// Internal method used for building the tokens.
	private void build(Message compoundMessage) throws IOException 
	{			
		int startIndex = 0;
		int indexOfMagicNumber = 0;
		
		BBDecoder decoder = new BBDecoder();
		decoder.init(compoundMessage.readData());
		byte [] source = decoder.readReaminingBytes();			
		
		int howManyTokens = 1;
		
		while ((indexOfMagicNumber = indexOf(source, startIndex+1)) != -1)
		{
			addMessageToken(source, startIndex, (indexOfMagicNumber-startIndex));
			startIndex = indexOfMagicNumber;
			howManyTokens++;
		}
		addMessageToken(source, startIndex, (source.length-startIndex));
		_iterator = _messages.iterator();
		
		LOGGER.debug(Messages.QMAN_200031_COMPOUND_MESSAGE_CONTAINS,howManyTokens);
	};

	// Builds & adds a new "message" token
	private void addMessageToken(byte [] source,int startIndex,int length) throws IOException
	{
		byte [] messageData = new byte[length];
		System.arraycopy(source, startIndex, messageData, 0, messageData.length);
		Message message = new ByteBufferMessage();
		message.appendData(messageData);
		_messages.add(message);		
	}
}
