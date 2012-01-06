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
package org.apache.qpid.client.message;

import java.io.UnsupportedEncodingException;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;

import javax.jms.JMSException;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.AMQException;
import org.apache.qpid.client.CustomJMSXProperty;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.util.Strings;

public class JMSTextMessage extends AbstractJMSMessage implements javax.jms.TextMessage
{
    private static final String MIME_TYPE = "text/plain";

    private String _decodedValue;

    /**
     * This constant represents the name of a property that is set when the message payload is null.
     */
    private static final String PAYLOAD_NULL_PROPERTY = CustomJMSXProperty.JMS_AMQP_NULL.toString();
    private static final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");

    public JMSTextMessage(AMQMessageDelegateFactory delegateFactory) throws JMSException
    {
        this(delegateFactory, null, null);
    }

    JMSTextMessage(AMQMessageDelegateFactory delegateFactory, ByteBuffer data, String encoding) throws JMSException
    {
        super(delegateFactory, data); // this instantiates a content header
        setContentType(getMimeType());
        setEncoding(encoding);
    }

    JMSTextMessage(AMQMessageDelegate delegate, ByteBuffer data)
            throws AMQException
    {
        super(delegate, data);
        setContentType(getMimeType());
        _data = data;
    }


    public void clearBodyImpl() throws JMSException
    {
        if (_data != null)
        {
            _data.release();
            _data = null;
        }

        _decodedValue = null;
    }

    public String toBodyString() throws JMSException
    {
        return getText();
    }

    protected String getMimeType()
    {
        return MIME_TYPE;
    }

    public void setText(String text) throws JMSException
    {
        checkWritable();

        clearBody();
        try
        {
            if (text != null)
            {
                final String encoding = getEncoding();
                if (encoding == null || encoding.equalsIgnoreCase("UTF-8"))
                {
                    _data = ByteBuffer.wrap(Strings.toUTF8(text));
                    setEncoding("UTF-8");
                }
                else
                {
                    _data = ByteBuffer.wrap(text.getBytes(encoding));
                }
                _data.position(_data.limit());
                _changedData=true;
            }
            _decodedValue = text;
        }
        catch (UnsupportedEncodingException e)
        {
            // should never occur
            JMSException jmse = new JMSException("Unable to decode text data");
            jmse.setLinkedException(e);
            throw jmse;
        }
    }

    public String getText() throws JMSException
    {
        if (_data == null && _decodedValue == null)
        {
            return null;
        }
        else if (_decodedValue != null)
        {
            return _decodedValue;
        }
        else
        {
            _data.rewind();

            if (propertyExists(PAYLOAD_NULL_PROPERTY) && getBooleanProperty(PAYLOAD_NULL_PROPERTY))
            {
                return null;
            }
            if (getEncoding() != null)
            {
                try
                {
                    _decodedValue = _data.getString(Charset.forName(getEncoding()).newDecoder());
                }
                catch (CharacterCodingException e)
                {
                    JMSException je = new JMSException("Could not decode string data: " + e);
                    je.setLinkedException(e);
                    throw je;
                }
            }
            else
            {
                try
                {
                    _decodedValue = _data.getString(DEFAULT_CHARSET.newDecoder());
                }
                catch (CharacterCodingException e)
                {
                    JMSException je = new JMSException("Could not decode string data: " + e);
                    je.setLinkedException(e);
                    throw je;
                }
            }
            return _decodedValue;
        }
    }

    @Override
    public void prepareForSending() throws JMSException
    {
        super.prepareForSending();
        if (_data == null)
        {
            setBooleanProperty(PAYLOAD_NULL_PROPERTY, true);
        }
        else
        {
            removeProperty(PAYLOAD_NULL_PROPERTY);
        }
    }


}
