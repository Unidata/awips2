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
package org.apache.qpid.framing;

import org.apache.qpid.AMQException;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

public class ProtocolInitiation extends AMQDataBlock implements EncodableAMQDataBlock
{

    // TODO: generate these constants automatically from the xml protocol spec file
    public static final byte[] AMQP_HEADER = new byte[]{(byte)'A',(byte)'M',(byte)'Q',(byte)'P'};

    private static final byte CURRENT_PROTOCOL_CLASS = 1;
    private static final byte TCP_PROTOCOL_INSTANCE = 1;

    public final byte[] _protocolHeader;
    public final byte _protocolClass;
    public final byte _protocolInstance;
    public final byte _protocolMajor;
    public final byte _protocolMinor;


//    public ProtocolInitiation() {}

    public ProtocolInitiation(byte[] protocolHeader, byte protocolClass, byte protocolInstance, byte protocolMajor, byte protocolMinor)
    {
        _protocolHeader = protocolHeader;
        _protocolClass = protocolClass;
        _protocolInstance = protocolInstance;
        _protocolMajor = protocolMajor;
        _protocolMinor = protocolMinor;
    }
    
    public ProtocolInitiation(ProtocolVersion pv)
    {
        this(AMQP_HEADER,
             pv.equals(ProtocolVersion.v0_91) ? 0 : CURRENT_PROTOCOL_CLASS,
             pv.equals(ProtocolVersion.v0_91) ? 0 : TCP_PROTOCOL_INSTANCE,
             pv.equals(ProtocolVersion.v0_91) ? 9 : pv.getMajorVersion(),
             pv.equals(ProtocolVersion.v0_91) ? 1 : pv.getMinorVersion());
    }

    public ProtocolInitiation(ByteBuffer in)
    {
        _protocolHeader = new byte[4];
        in.get(_protocolHeader);

        _protocolClass = in.get();
        _protocolInstance = in.get();
        _protocolMajor = in.get();
        _protocolMinor = in.get();
    }

    public void writePayload(org.apache.mina.common.ByteBuffer buffer)
    {
        writePayload(buffer.buf());
    }
    
    public long getSize()
    {
        return 4 + 1 + 1 + 1 + 1;
    }

    public void writePayload(ByteBuffer buffer)
    {

        buffer.put(_protocolHeader);
        buffer.put(_protocolClass);
        buffer.put(_protocolInstance);
        buffer.put(_protocolMajor);
        buffer.put(_protocolMinor);
    }

    public boolean equals(Object o)
    {
        if (!(o instanceof ProtocolInitiation))
        {
            return false;
        }

        ProtocolInitiation pi = (ProtocolInitiation) o;
        if (pi._protocolHeader == null)
        {
            return false;
        }

        if (_protocolHeader.length != pi._protocolHeader.length)
        {
            return false;
        }

        for (int i = 0; i < _protocolHeader.length; i++)
        {
            if (_protocolHeader[i] != pi._protocolHeader[i])
            {
                return false;
            }
        }

        return (_protocolClass == pi._protocolClass &&
                _protocolInstance == pi._protocolInstance &&
                _protocolMajor == pi._protocolMajor &&
                _protocolMinor == pi._protocolMinor);
    }

    public static class Decoder //implements MessageDecoder
    {
        /**
         *
         * @param in input buffer
         * @return true if we have enough data to decode the PI frame fully, false if more
         * data is required
         */
        public boolean decodable(ByteBuffer in)
        {
            return (in.remaining() >= 8);
        }

    }

    public ProtocolVersion checkVersion() throws AMQException
    {

        if(_protocolHeader.length != 4)
        {
            throw new AMQProtocolHeaderException("Protocol header should have exactly four octets", null);
        }
        for(int i = 0; i < 4; i++)
        {
            if(_protocolHeader[i] != AMQP_HEADER[i])
            {
                try
                {
                    throw new AMQProtocolHeaderException("Protocol header is not correct: Got " + new String(_protocolHeader,"ISO-8859-1") + " should be: " + new String(AMQP_HEADER, "ISO-8859-1"), null);
                }
                catch (UnsupportedEncodingException e)
                {
                    
                }
            }
        }

        ProtocolVersion pv;

        // Hack for 0-9-1 which changed how the header was defined
        if(_protocolInstance == 0 && _protocolMajor == 9 && _protocolMinor == 1)
        {
            pv = ProtocolVersion.v0_91;
            if (_protocolClass != 0)
            {
                throw new AMQProtocolClassException("Protocol class " + 0 + " was expected; received " +
                                                    _protocolClass, null);
            }
        }
        else if (_protocolClass != CURRENT_PROTOCOL_CLASS)
        {
            throw new AMQProtocolClassException("Protocol class " + CURRENT_PROTOCOL_CLASS + " was expected; received " +
                                                _protocolClass, null);
        }
        else if (_protocolInstance != TCP_PROTOCOL_INSTANCE)
        {
            throw new AMQProtocolInstanceException("Protocol instance " + TCP_PROTOCOL_INSTANCE + " was expected; received " +
                                                   _protocolInstance, null);
        }
        else
        {
            pv = new ProtocolVersion(_protocolMajor, _protocolMinor);
        }
        

        if (!pv.isSupported())
        {
            // TODO: add list of available versions in list to msg...
            throw new AMQProtocolVersionException("Protocol version " +
                                                  _protocolMajor + "." + _protocolMinor + " not suppoerted by this version of the Qpid broker.", null);
        }
        return pv;
    }

    public String toString()
    {
        StringBuffer buffer = new StringBuffer(new String(_protocolHeader));
        buffer.append(Integer.toHexString(_protocolClass));
        buffer.append(Integer.toHexString(_protocolInstance));
        buffer.append(Integer.toHexString(_protocolMajor));
        buffer.append(Integer.toHexString(_protocolMinor));
        return buffer.toString();
    }

}
