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

import java.io.IOException;
import java.nio.ByteBuffer;

import org.apache.qpid.api.Message;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.domain.services.SequenceNumberGenerator;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.Header;
import org.apache.qpid.transport.MessageProperties;
import org.apache.qpid.transport.codec.BBEncoder;

/**
 * Message implementation used for specific management purposes.
 * 
 * @author Andrea Gazzarini
 */
public abstract class ManagementMessage implements Message
{
    /**
     * Strategy interface for building / getting data.
     * 
     * @author Andrea Gazzarini
     */
    private interface IDataBuilderStrategy 
    {
       ByteBuffer getData(); 
    };
    
    /**
     * Strategy used for retrieving raw data from this message when it has been already encoded.
     */
    IDataBuilderStrategy READING = new IDataBuilderStrategy()
    {
        public ByteBuffer getData() {
            return _data;
        };
    };
    
    /**
     * Strategy used for retrieving raw data from this message when it hasn't been already encoded.
     */
    IDataBuilderStrategy ACCUMULATING = new IDataBuilderStrategy()
    {
        public ByteBuffer getData() {
        	_codec.writeInt8((byte)opcode());
            _codec.writeSequenceNo(sequenceNumber());
            
            specificMessageEncoding();
            
            _data =_codec.segment(); 
            _reader = READING;
            return _data;
        }
    };    
    
    protected BBEncoder _codec;
    protected ByteBuffer _data;
    private int _messageTransferId;
    private IDataBuilderStrategy _reader = ACCUMULATING;    
    
    /**
     * Builds an empty  management message.
     */
    ManagementMessage()
    {
        _codec = new BBEncoder(100);
        _codec.writeMagicNumber();
    }

    /**
     * Returns the sequence number that will be used for this message.
     * 
     * @return the sequence number that will be used for this message.
     */
    protected int sequenceNumber ()
    {
        return SequenceNumberGenerator.getNextSequenceNumber();
    }

    /**
     * Returns the opcode that will be used for this message.
     * 
     * @return the opcode that will be used for this message.
     */
    abstract char opcode ();

    /**
     * Returns the delivery properties of this message.
     * 
     * @return the delivery properties of this message.
     */
    public DeliveryProperties getDeliveryProperties ()
    {
        return Configuration.getInstance().getCommandDeliveryProperties();
    }

    /**
     * Returns the header of this message.
     * 
     * @return the header of this message.
     */
    public Header getHeader ()
    {
        return Configuration.getInstance().getCommandMessageHeader();
    }

    /**
     * Returns the messages header properties of this message.
     * 
     * @return the message header properties of this message.
     */
    public MessageProperties getMessageProperties ()
    {
        return Configuration.getInstance().getCommandMessageProperties();
    }
    
    /**
     * Returns the transfer Id of this message.
     * 
     * @return the transfer Id of this message.
     */
    public int getMessageTransferId ()
    {
        return _messageTransferId;
    }

    /**
     * Returns the encoded data of this message.
     * 
     * @return the encoded data of this message.
     */
    public ByteBuffer readData () throws IOException
    {
        return _reader.getData();
    }

    /**
     * Sets the header for this message.
     * 
     * @param header the new message header.
     */
    public void setHeader (Header header)
    {
       // N.A. at the moment.
    }

    public void appendData (byte[] src) throws IOException
    {
    }

    public void appendData (ByteBuffer src) throws IOException
    {
    }

    public void clearData ()
    {
    }

    public void readData (byte[] target) throws IOException
    {
    }
    
    /**
     * Concrete subclasses (message implementations) must define here their specific data encoding.
     */
    abstract void specificMessageEncoding();    
}