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
package org.apache.qpid.management.messages;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.domain.model.QpidMethod;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.Header;
import org.apache.qpid.transport.MessageProperties;
import org.apache.qpid.transport.ReplyTo;
import org.apache.qpid.transport.util.Logger;

/**
 * Abstract representation of a method invocation request message.
 * Concrete subclasses must supply the values needed to build & encode the message.
 * 
 * @author Andrea Gazzarini
 */
public abstract class MethodInvocationRequestMessage extends ManagementMessage
{
	private final static Logger LOGGER = Logger.get(MethodInvocationRequestMessage.class);
	
	private DeliveryProperties _deliveryProperties;
	private MessageProperties _messageProperties;
	private Header _header;	
	
	/**
	 * Builds a new method invocation request message with the given target identifiers.
	 * 
	 * @param bankId the bank identifier.
	 * @param brokerId the broker identifier.
	 */
	public MethodInvocationRequestMessage(long bankId, long brokerId) 
	{
		 ReplyTo replyTo=new ReplyTo();
	     replyTo.setRoutingKey(Configuration.getInstance().getMethodReplyQueueName());
	     _messageProperties = new MessageProperties();
	     _messageProperties.setReplyTo(replyTo);

	     String routingKey = String.format(Names.AGENT_ROUTING_KEY_PREFIX+"%s.%s", brokerId,bankId);
	     
	     LOGGER.debug(Messages.QMAN_200032_COMMAND_MESSAGE_ROUTING_KEY, routingKey);
	     
	     _deliveryProperties = new DeliveryProperties();	     
	     _deliveryProperties.setRoutingKey(routingKey);        
	     _header = new Header(_deliveryProperties, _messageProperties);
	}
	
    @Override
    char opcode ()
    {
        return Protocol.OPERATION_INVOCATION_REQUEST_OPCODE;
    }

    /**
     * Returns the package name.
     * 
     * @return the package name.
     */
    protected abstract String packageName();
    
    /**
     * Returns the class name.
     * 
     * @return the class name.
     */
    protected abstract String className();
    
    /**
     * Returns the schema hash.
     * 
     * @return the schema hash.
     */
    protected abstract Binary schemaHash();
    
    /**
     * Returns the object identifier.
     * 
     * @return the object identifier.
     */
    protected abstract Binary objectId();
    
    /**
     * Returns the method to be invoked.
     * 
     * @return the method to be invoked.
     */
    protected abstract QpidMethod method();
    
    /**
     * Returns the parameters used for method invocation.
     * 
     * @return the parameters used for method invocation.
     */
    protected abstract Object[] parameters();

    /**
     * Returns the delivery properties of this message.
     * 
     * @return the delivery properties of this message.
     */
    public DeliveryProperties getDeliveryProperties ()
    {
        return _deliveryProperties;
    }

    /**
     * Returns the header of this message.
     * 
     * @return the header of this message.
     */
    public Header getHeader ()
    {
        return _header;
    }

    /**
     * Returns the messages header properties of this message.
     * 
     * @return the message header properties of this message.
     */
    public MessageProperties getMessageProperties ()
    {
        return _messageProperties;
    }
    
    @Override
    void specificMessageEncoding ()
    {
        objectId().encode(_codec);
        _codec.writeStr8(packageName());
        _codec.writeStr8(className());        
        schemaHash().encode(_codec);
        
        QpidMethod method = method();
        _codec.writeStr8(method.getName());
       method.encodeParameters(parameters(), _codec);
    }
}
