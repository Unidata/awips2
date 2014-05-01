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
package org.apache.qpid.client;

import java.net.URISyntaxException;

import javax.jms.Destination;
import javax.naming.NamingException;
import javax.naming.Reference;
import javax.naming.Referenceable;
import javax.naming.StringRefAddr;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.url.AMQBindingURL;
import org.apache.qpid.url.BindingURL;
import org.apache.qpid.url.URLHelper;


public abstract class AMQDestination implements Destination, Referenceable
{
    protected final AMQShortString _exchangeName;

    protected final AMQShortString _exchangeClass;

    protected final boolean _isDurable;

    protected final boolean _isExclusive;

    protected final boolean _isAutoDelete;

    private AMQShortString _queueName;

    private AMQShortString _routingKey;

    private AMQShortString[] _bindingKeys;

    private String _url;
    private AMQShortString _urlAsShortString;

    private boolean _checkedForQueueBinding;

    private boolean _exchangeExistsChecked;

    private byte[] _byteEncoding;
    private static final int IS_DURABLE_MASK = 0x1;
    private static final int IS_EXCLUSIVE_MASK = 0x2;
    private static final int IS_AUTODELETE_MASK = 0x4;

    public static final int QUEUE_TYPE = 1;
    public static final int TOPIC_TYPE = 2;
    public static final int UNKNOWN_TYPE = 3;

    protected AMQDestination(String url) throws URISyntaxException
    {
        this(new AMQBindingURL(url));
    }

    protected AMQDestination(BindingURL binding)
    {
        _exchangeName = binding.getExchangeName();
        _exchangeClass = binding.getExchangeClass();

        _isExclusive = Boolean.parseBoolean(binding.getOption(BindingURL.OPTION_EXCLUSIVE));
        _isAutoDelete = Boolean.parseBoolean(binding.getOption(BindingURL.OPTION_AUTODELETE));
        _isDurable = Boolean.parseBoolean(binding.getOption(BindingURL.OPTION_DURABLE));
        _queueName = binding.getQueueName() == null ? null : binding.getQueueName();
        _routingKey = binding.getRoutingKey() == null ? null : binding.getRoutingKey();
        _bindingKeys = binding.getBindingKeys() == null || binding.getBindingKeys().length == 0 ? new AMQShortString[0] : binding.getBindingKeys();
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, AMQShortString queueName)
    {
        this(exchangeName, exchangeClass, routingKey, false, false, queueName, null);
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, AMQShortString queueName, AMQShortString[] bindingKeys)
    {
        this(exchangeName, exchangeClass, routingKey, false, false, queueName,bindingKeys);
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString destinationName)
    {
        this(exchangeName, exchangeClass, destinationName, false, false, null,null);
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, boolean isExclusive,
            boolean isAutoDelete, AMQShortString queueName)
    {
        this(exchangeName, exchangeClass, routingKey, isExclusive, isAutoDelete, queueName, false,null);
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, boolean isExclusive,
                             boolean isAutoDelete, AMQShortString queueName,AMQShortString[] bindingKeys)
    {
        this(exchangeName, exchangeClass, routingKey, isExclusive, isAutoDelete, queueName, false,bindingKeys);
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, boolean isExclusive,
            boolean isAutoDelete, AMQShortString queueName, boolean isDurable){
        this (exchangeName, exchangeClass, routingKey, isExclusive,isAutoDelete,queueName,isDurable,null);
    }

    protected AMQDestination(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, boolean isExclusive,
                             boolean isAutoDelete, AMQShortString queueName, boolean isDurable,AMQShortString[] bindingKeys)
    {
        if ( (ExchangeDefaults.DIRECT_EXCHANGE_CLASS.equals(exchangeClass) || 
              ExchangeDefaults.TOPIC_EXCHANGE_CLASS.equals(exchangeClass))  
              && routingKey == null)
        {
            throw new IllegalArgumentException("routing/binding key  must not be null");
        }
        if (exchangeName == null)
        {
            throw new IllegalArgumentException("Exchange name must not be null");
        }
        if (exchangeClass == null)
        {
            throw new IllegalArgumentException("Exchange class must not be null");
        }
        _exchangeName = exchangeName;
        _exchangeClass = exchangeClass;
        _routingKey = routingKey;
        _isExclusive = isExclusive;
        _isAutoDelete = isAutoDelete;
        _queueName = queueName;
        _isDurable = isDurable;
        _bindingKeys = bindingKeys == null || bindingKeys.length == 0 ? new AMQShortString[0] : bindingKeys;
    }

    public AMQShortString getEncodedName()
    {
        if(_urlAsShortString == null)
        {
            toURL();
        }
        return _urlAsShortString;
    }

    public boolean isDurable()
    {
        return _isDurable;
    }

    public AMQShortString getExchangeName()
    {
        return _exchangeName;
    }

    public AMQShortString getExchangeClass()
    {
        return _exchangeClass;
    }

    public boolean isTopic()
    {
        return ExchangeDefaults.TOPIC_EXCHANGE_CLASS.equals(_exchangeClass);
    }

    public boolean isQueue()
    {
        return ExchangeDefaults.DIRECT_EXCHANGE_CLASS.equals(_exchangeClass);
    }

    public String getQueueName()
    {
        return _queueName == null ? null : _queueName.toString();
    }

    public AMQShortString getAMQQueueName()
    {
        return _queueName;
    }

    public void setQueueName(AMQShortString queueName)
    {

        _queueName = queueName;
        // calculated URL now out of date
        _url = null;
        _urlAsShortString = null;
        _byteEncoding = null;
    }

    public AMQShortString getRoutingKey()
    {
        return _routingKey;
    }

    public AMQShortString[] getBindingKeys()
    {
        if (_bindingKeys != null && _bindingKeys.length > 0)
        {
            return _bindingKeys;
        }
        else
        {
            // catering to the common use case where the
            //routingKey is the same as the bindingKey.
            return new AMQShortString[]{_routingKey};
        }
    }

    public boolean isExclusive()
    {
        return _isExclusive;
    }

    public boolean isAutoDelete()
    {
        return _isAutoDelete;
    }

    public abstract boolean isNameRequired();

    public String toString()
    {
        return toURL();

    }

    public boolean isCheckedForQueueBinding()
    {
        return _checkedForQueueBinding;
    }

    public void setCheckedForQueueBinding(boolean checkedForQueueBinding)
    {
        _checkedForQueueBinding = checkedForQueueBinding;
    }


    public boolean isExchangeExistsChecked()
    {
        return _exchangeExistsChecked;
    }

    public void setExchangeExistsChecked(final boolean exchangeExistsChecked)
    {
        _exchangeExistsChecked = exchangeExistsChecked;
    }

    public String toURL()
    {
        String url = _url;
        if(url == null)
        {


            StringBuffer sb = new StringBuffer();

            sb.append(_exchangeClass);
            sb.append("://");
            sb.append(_exchangeName);

            sb.append("/"+_routingKey+"/");

            if (_queueName != null)
            {
                sb.append(_queueName);
            }

            sb.append('?');

            if (_routingKey != null)
            {
                sb.append(BindingURL.OPTION_ROUTING_KEY);
                sb.append("='");
                sb.append(_routingKey).append("'");
                sb.append(URLHelper.DEFAULT_OPTION_SEPERATOR);
            }

            // We can't allow both routingKey and bindingKey
            if (_routingKey == null && _bindingKeys != null && _bindingKeys.length>0)
            {

                for (AMQShortString bindingKey:_bindingKeys)
                {
                    sb.append(BindingURL.OPTION_BINDING_KEY);
                    sb.append("='");
                    sb.append(bindingKey);
                    sb.append("'");
                    sb.append(URLHelper.DEFAULT_OPTION_SEPERATOR);

                }
            }

            if (_isDurable)
            {
                sb.append(BindingURL.OPTION_DURABLE);
                sb.append("='true'");
                sb.append(URLHelper.DEFAULT_OPTION_SEPERATOR);
            }

            if (_isExclusive)
            {
                sb.append(BindingURL.OPTION_EXCLUSIVE);
                sb.append("='true'");
                sb.append(URLHelper.DEFAULT_OPTION_SEPERATOR);
            }

            if (_isAutoDelete)
            {
                sb.append(BindingURL.OPTION_AUTODELETE);
                sb.append("='true'");
                sb.append(URLHelper.DEFAULT_OPTION_SEPERATOR);
            }

            //removeKey the last char '?' if there is no options , ',' if there are.
            sb.deleteCharAt(sb.length() - 1);
            url = sb.toString();
            _url = url;
            _urlAsShortString = new AMQShortString(url);
        }
        return url;
    }

    public byte[] toByteEncoding()
    {
        byte[] encoding = _byteEncoding;
        if(encoding == null)
        {
            int size = _exchangeClass.length() + 1 +
                       _exchangeName.length() + 1 +
                       0 +  // in place of the destination name
                       (_queueName == null ? 0 : _queueName.length()) + 1 +
                       1;
            encoding = new byte[size];
            int pos = 0;

            pos = _exchangeClass.writeToByteArray(encoding, pos);
            pos = _exchangeName.writeToByteArray(encoding, pos);

            encoding[pos++] = (byte)0;

            if(_queueName == null)
            {
                encoding[pos++] = (byte)0;
            }
            else
            {
                pos = _queueName.writeToByteArray(encoding,pos);
            }
            byte options = 0;
            if(_isDurable)
            {
                options |= IS_DURABLE_MASK;
            }
            if(_isExclusive)
            {
                options |= IS_EXCLUSIVE_MASK;
            }
            if(_isAutoDelete)
            {
                options |= IS_AUTODELETE_MASK;
            }
            encoding[pos] = options;


            _byteEncoding = encoding;

        }
        return encoding;
    }

    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        final AMQDestination that = (AMQDestination) o;

        if (!_exchangeClass.equals(that._exchangeClass))
        {
            return false;
        }
        if (!_exchangeName.equals(that._exchangeName))
        {
            return false;
        }
        if ((_queueName == null && that._queueName != null) ||
            (_queueName != null && !_queueName.equals(that._queueName)))
        {
            return false;
        }

        return true;
    }

    public int hashCode()
    {
        int result;
        result = _exchangeName.hashCode();
        result = 29 * result + _exchangeClass.hashCode();
        //result = 29 * result + _destinationName.hashCode();
        if (_queueName != null)
        {
            result = 29 * result + _queueName.hashCode();
        }

        return result;
    }

    public Reference getReference() throws NamingException
    {
        return new Reference(
                this.getClass().getName(),
                new StringRefAddr(this.getClass().getName(), toURL()),
                AMQConnectionFactory.class.getName(),
                null);          // factory location
    }


    public static Destination createDestination(byte[] byteEncodedDestination)
    {
        AMQShortString exchangeClass;
        AMQShortString exchangeName;
        AMQShortString routingKey;
        AMQShortString queueName;
        boolean isDurable;
        boolean isExclusive;
        boolean isAutoDelete;

        int pos = 0;
        exchangeClass = AMQShortString.readFromByteArray(byteEncodedDestination, pos);
        pos+= exchangeClass.length() + 1;
        exchangeName =  AMQShortString.readFromByteArray(byteEncodedDestination, pos);
        pos+= exchangeName.length() + 1;
        routingKey =  AMQShortString.readFromByteArray(byteEncodedDestination, pos);
        pos+= (routingKey == null ? 0 : routingKey.length()) + 1;
        queueName =  AMQShortString.readFromByteArray(byteEncodedDestination, pos);
        pos+= (queueName == null ? 0 : queueName.length()) + 1;
        int options = byteEncodedDestination[pos];
        isDurable = (options & IS_DURABLE_MASK) != 0;
        isExclusive = (options & IS_EXCLUSIVE_MASK) != 0;
        isAutoDelete = (options & IS_AUTODELETE_MASK) != 0;

        if (exchangeClass.equals(ExchangeDefaults.DIRECT_EXCHANGE_CLASS))
        {
            return new AMQQueue(exchangeName,routingKey,queueName,isExclusive,isAutoDelete,isDurable);
        }
        else if (exchangeClass.equals(ExchangeDefaults.TOPIC_EXCHANGE_CLASS))
        {
            return new AMQTopic(exchangeName,routingKey,isAutoDelete,queueName,isDurable);
        }
        else if (exchangeClass.equals(ExchangeDefaults.HEADERS_EXCHANGE_CLASS))
        {
            return new AMQHeadersExchange(routingKey);
        }
        else
        {
            return new AMQAnyDestination(exchangeName,exchangeClass,
                                         routingKey,isExclusive, 
                                         isAutoDelete,queueName, 
                                         isDurable, new AMQShortString[0]);
        }

    }

    public static Destination createDestination(BindingURL binding)
    {
        AMQShortString type = binding.getExchangeClass();

        if (type.equals(ExchangeDefaults.DIRECT_EXCHANGE_CLASS))
        {
            return new AMQQueue(binding);
        }
        else if (type.equals(ExchangeDefaults.TOPIC_EXCHANGE_CLASS))
        {
            return new AMQTopic(binding);
        }
        else if (type.equals(ExchangeDefaults.HEADERS_EXCHANGE_CLASS))
        {
            return new AMQHeadersExchange(binding);
        }
        else
        {
            return new AMQAnyDestination(binding);
        }
    }
}
