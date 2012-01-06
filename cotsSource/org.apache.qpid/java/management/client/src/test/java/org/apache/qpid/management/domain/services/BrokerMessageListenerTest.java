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
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import junit.framework.TestCase;

import org.apache.qpid.api.Message;
import org.apache.qpid.management.TestConstants;
import org.apache.qpid.management.domain.handler.base.IMessageHandler;
import org.apache.qpid.management.domain.model.DomainModel;
import org.apache.qpid.nclient.util.ByteBufferMessage;
import org.apache.qpid.transport.codec.Decoder;

/**
 * Test case for Broker Message Listener.
 * 
 * @author Andrea Gazzarini
 */
public class BrokerMessageListenerTest extends TestCase
{
    // An empty message handler user for test.
    private IMessageHandler _emptyMessageHandler = new IMessageHandler() 
    {
        public void process (Decoder decoder, int sequenceNumber)
        {
        }
        public void setDomainModel (DomainModel domainModel) 
        {
        }
    };

    // Another empty message handler user for test.
    private IMessageHandler _anotherEmptyMessageHandler = new IMessageHandler() 
    {
        public void process (Decoder decoder, int sequenceNumber)
        {
        }
        public void setDomainModel (DomainModel domainModel)
        {
        }
    };

    private Map<Character,IMessageHandler> _handlers = new HashMap<Character, IMessageHandler>();
    private BrokerMessageListener _listener;
    private final char opcode1 = 'x';
    private final char opcode2 = 'y';
    
    
    @Override
    protected void setUp () throws Exception
    {
        DomainModel domainModel = new DomainModel(TestConstants.BROKER_ID);
        _listener = new BrokerMessageListener(domainModel);
        
        _handlers.put(opcode1, _emptyMessageHandler);
        _handlers.put(opcode2, _anotherEmptyMessageHandler);
    }
    
    /**
     * Tests the installation of message handlers on a broker message listener.
     * 
     * <br>precondition : no message handler has been installed on message listener.
     * <br>postcondition : two message handlers are installed on message listener.
     */
    public void testSetHandlersOK() 
    {
        assertTrue(
                "No handler has yet been installed so how is it possible that the handlers map is not empty?",
                _listener._handlers.isEmpty());
        
        _listener.setHandlers(_handlers);
        
        assertEquals("Now we should have two handlers configured.",2,_listener._handlers.size());
        assertSame(_listener._handlers.get(opcode1),_emptyMessageHandler);
        assertSame(_listener._handlers.get(opcode2),_anotherEmptyMessageHandler);
    }
    
    /**
     * Tests the installation of message handlers on a broker message listener. 
     * Specifically it tries to install three message handlers and one of them is throwing an exception at installation time.
     * 
     * <br>precondition : no message handler has been installed on message listener.
     * <br>postcondition : two message handlers are installed on message listener. (the one that thrown exception has been
     * discarded).
     */
    public void testSetHandlerOK() 
    {
        IMessageHandler wrongMessageHandler = new IMessageHandler()
        {

            public void process (Decoder decoder, int sequenceNumber)
            {
            }

            public void setDomainModel (DomainModel domainModel)
            {
                throw new RuntimeException();
            }  
        };
        
        char opcodeForWrongHandler = 'k';
        
        assertTrue(
                "No handler has yet been installed so how is it possible that the handlers map is not empty?",
                _listener._handlers.isEmpty());
        
        _handlers.put(opcodeForWrongHandler,wrongMessageHandler);

        _listener.setHandlers(_handlers);
        
        assertEquals("Now we should have two handlers configured.",2,_listener._handlers.size());
        assertSame(_listener._handlers.get(opcode1),_emptyMessageHandler);
        assertSame(_listener._handlers.get(opcode2),_anotherEmptyMessageHandler);
        assertNull(_listener._handlers.get(opcodeForWrongHandler));
    }
    
    /**
     * Tests the execution of the onMessage() method when a message with a bad magic number is received.
     * 
     * <br>precondition : a message with a bad magic number is received.
     * <br>postcondition : the processing of the incoming message is skipped and therefore no handler will be called.
     */
    public void testOnMessageKO_withBadMagicNumber() throws IOException 
    {    
        IMessageHandler neverCallMe = new IMessageHandler()
        {

            public void process (Decoder decoder, int sequenceNumber)
            {
                fail("This test shouldn't never arrive at this point...");
            }

            public void setDomainModel (DomainModel domainModel)
            {
            }  
        };
        
        String opcodeForNeverCallMeHandler = "w";
        
        _handlers.put('w',neverCallMe);
        _listener.setHandlers(_handlers);
        
        Message message = new ByteBufferMessage();
        message.appendData( ("AMG"+opcodeForNeverCallMeHandler).getBytes());
        
        _listener.onMessage(message);
    }
    
    /**
     * Tests the execution of the onMessage() method when the incoming message is a compound message.
     * 
     * <br>precondition : the incoming message is a compound message.
     * <br>postcondition : each tokenized message is forwarded to the appropriate handler.
     */
    public void testOnMessageOK_WithCompoundMessage() throws Exception 
    {
    	final Map<Character,IMessageHandler> handlersMap = new HashMap<Character,IMessageHandler>();
    	char [] opcodes = {'a','b','c','d','e'};
    	
    	class MockMessageHandler implements IMessageHandler
        {
    		private final char _opcode;
    		
    		public MockMessageHandler(char opcode) 
    		{
    			this._opcode = opcode;
			}
    		
            public void process (Decoder decoder, int sequenceNumber)
            {
            	handlersMap.remove(_opcode);
            }

            public void setDomainModel (DomainModel domainModel)
            {
            	// Do nothing here. It's just a mock handler.
            }      	
        };
    	
    	for (char opcode : opcodes) 
    	{
        	handlersMap.put(opcode, new MockMessageHandler(opcode));			
		}
    	        
    	// Removes previously injected handlers (i.e. x & y)
    	_listener._handlers.clear();
        _listener.setHandlers(handlersMap);
        
        Message compoundMessage = createCompoundMessage(opcodes);
        _listener.onMessage(compoundMessage);
        
        assertTrue(handlersMap.isEmpty());
    }

    // Creates a (non valid) compound message.
	private Message createCompoundMessage(char[] opcodes) throws IOException {
		byte [] compoundMessageData = new byte [12 * opcodes.length];
		Random randomizer = new Random();
		int position = 0;
		
		for (char opcode : opcodes) {
			System.arraycopy(MessageTokenizer.MAGIC_NUMBER_BYTES, 0, compoundMessageData, position, MessageTokenizer.MAGIC_NUMBER_BYTES.length);
			position+=MessageTokenizer.MAGIC_NUMBER_BYTES.length;
			
			compoundMessageData[position++] = (byte)opcode;
			
			for (int c = 4; c < 12; c++) 
			{
				byte aByte = (byte)randomizer.nextInt(127);
				compoundMessageData[position++] = aByte;
			}
		}
						
		Message compoundMessage = new ByteBufferMessage();
		compoundMessage.appendData(compoundMessageData);
		return compoundMessage;
	}
}
