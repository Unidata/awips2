package org.apache.qpid.url;
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


import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BindingURLParser
{
    private static final char PROPERTY_EQUALS_CHAR = '=';
    private static final char PROPERTY_SEPARATOR_CHAR = '&';
    private static final char ALTERNATIVE_PROPERTY_SEPARATOR_CHAR = ',';
    private static final char FORWARD_SLASH_CHAR = '/';
    private static final char QUESTION_MARK_CHAR = '?';
    private static final char SINGLE_QUOTE_CHAR = '\'';
    private static final char COLON_CHAR = ':';
    private static final char END_OF_URL_MARKER_CHAR = '%';

    private static final Logger _logger = LoggerFactory.getLogger(BindingURLParser.class);

    private char[] _url;
    private AMQBindingURL _bindingURL;
    private BindingURLParserState _currentParserState;
    private String _error;
    private int _index = 0;
    private String _currentPropName;
    private Map<String,Object> _options = new HashMap<String,Object>();

    //<exch_class>://<exch_name>/[<destination>]/[<queue>]?<option>='<value>'[,<option>='<value>']*
    public BindingURLParser(String url,AMQBindingURL bindingURL) throws URISyntaxException
    {
        _url = (url + END_OF_URL_MARKER_CHAR).toCharArray();
        _bindingURL = bindingURL;
        _currentParserState = BindingURLParserState.BINDING_URL_START;
        BindingURLParserState prevState = _currentParserState;

        try
        {
            while (_currentParserState != BindingURLParserState.ERROR && _currentParserState != BindingURLParserState.BINDING_URL_END)
            {
                prevState = _currentParserState;
                _currentParserState = next();
            }

            if (_currentParserState == BindingURLParserState.ERROR)
            {
                _error =
                        "Invalid URL format [current_state = " + prevState + ", details parsed so far " + _bindingURL + " ] error at (" + _index + ") due to " + _error;
                _logger.debug(_error);
                URISyntaxException ex;
                ex = new URISyntaxException(markErrorLocation(),"Error occured while parsing URL",_index);
                throw ex;
            }

            processOptions();
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
                _error = "Invalid URL format [current_state = " + prevState + ", details parsed so far " + _bindingURL + " ] error at (" + _index + ")";
                URISyntaxException ex = new URISyntaxException(markErrorLocation(),"Error occured while parsing URL",_index);
                ex.initCause(e);
                throw ex;
        }
    }

    enum BindingURLParserState
    {
        BINDING_URL_START,
        EXCHANGE_CLASS,
        COLON_CHAR,
        DOUBLE_SEP,
        EXCHANGE_NAME,
        EXCHANGE_SEPERATOR_CHAR,
        DESTINATION,
        DESTINATION_SEPERATOR_CHAR,
        QUEUE_NAME,
        QUESTION_MARK_CHAR,
        PROPERTY_NAME,
        PROPERTY_EQUALS,
        START_PROPERTY_VALUE,
        PROPERTY_VALUE,
        END_PROPERTY_VALUE,
        PROPERTY_SEPARATOR,
        BINDING_URL_END,
        ERROR
    }

    /**
     * I am fully ware that there are few optimizations
     * that can speed up things a wee bit. But I have opted
     * for readability and maintainability at the expense of
     * speed, as speed is not a critical factor here.
     *
     * One can understand the full parse logic by just looking at this method.
     */
    private BindingURLParserState next()
    {
        switch (_currentParserState)
        {
            case BINDING_URL_START:
                return extractExchangeClass();
            case COLON_CHAR:
                _index++; //skip ":"
                return BindingURLParserState.DOUBLE_SEP;
            case DOUBLE_SEP:
                _index = _index + 2; //skip "//"
                return BindingURLParserState.EXCHANGE_NAME;
            case EXCHANGE_NAME:
                return extractExchangeName();
            case EXCHANGE_SEPERATOR_CHAR:
                _index++; // skip '/'
                return BindingURLParserState.DESTINATION;
            case DESTINATION:
                return extractDestination();
            case DESTINATION_SEPERATOR_CHAR:
                _index++; // skip '/'
                return BindingURLParserState.QUEUE_NAME;
            case QUEUE_NAME:
                return extractQueueName();
            case QUESTION_MARK_CHAR:
                _index++; // skip '?'
                return BindingURLParserState.PROPERTY_NAME;
            case PROPERTY_NAME:
                return extractPropertyName();
            case PROPERTY_EQUALS:
                _index++; // skip the equal sign
                return BindingURLParserState.START_PROPERTY_VALUE;
            case START_PROPERTY_VALUE:
                _index++; // skip the '\''
                return BindingURLParserState.PROPERTY_VALUE;
            case PROPERTY_VALUE:
                return extractPropertyValue();
            case END_PROPERTY_VALUE:
                _index ++;
                return checkEndOfURL();
            case PROPERTY_SEPARATOR:
                _index++; // skip '&'
                return BindingURLParserState.PROPERTY_NAME;
            default:
                return BindingURLParserState.ERROR;
        }
    }

    private BindingURLParserState extractExchangeClass()
    {
        char nextChar = _url[_index];

        // check for the following special cases.
        // "myQueue?durable='true'" or just "myQueue";

        StringBuilder builder = new StringBuilder();
        while (nextChar != COLON_CHAR && nextChar != QUESTION_MARK_CHAR && nextChar != END_OF_URL_MARKER_CHAR)
        {
            builder.append(nextChar);
            _index++;
            nextChar = _url[_index];
        }

        // normal use case
        if (nextChar == COLON_CHAR)
        {
            _bindingURL.setExchangeClass(builder.toString());
            return BindingURLParserState.COLON_CHAR;
        }
        // "myQueue?durable='true'" use case
        else if (nextChar == QUESTION_MARK_CHAR)
        {
            _bindingURL.setExchangeClass(ExchangeDefaults.DIRECT_EXCHANGE_CLASS.asString());
            _bindingURL.setExchangeName("");
            _bindingURL.setQueueName(builder.toString());
            return BindingURLParserState.QUESTION_MARK_CHAR;
        }
        else
        {
            _bindingURL.setExchangeClass(ExchangeDefaults.DIRECT_EXCHANGE_CLASS.asString());
            _bindingURL.setExchangeName("");
            _bindingURL.setQueueName(builder.toString());
            return BindingURLParserState.BINDING_URL_END;
        }
    }

    private BindingURLParserState extractExchangeName()
    {
        char nextChar = _url[_index];
        StringBuilder builder = new StringBuilder();
        while (nextChar != FORWARD_SLASH_CHAR)
        {
            builder.append(nextChar);
            _index++;
            nextChar = _url[_index];
        }

        _bindingURL.setExchangeName(builder.toString());
        return BindingURLParserState.EXCHANGE_SEPERATOR_CHAR;
    }

    private BindingURLParserState extractDestination()
    {
        char nextChar = _url[_index];

        //The destination is and queue name are both optional
        // This is checking for the case where both are not specified.
        if (nextChar == QUESTION_MARK_CHAR)
        {
            return BindingURLParserState.QUESTION_MARK_CHAR;
        }

        StringBuilder builder = new StringBuilder();
        while (nextChar != FORWARD_SLASH_CHAR && nextChar != QUESTION_MARK_CHAR)
        {
            builder.append(nextChar);
            _index++;
            nextChar = _url[_index];
        }

        // This is the case where the destination is explictily stated.
        // ex direct://amq.direct/myDest/myQueue?option1='1' ... OR
        // direct://amq.direct//myQueue?option1='1' ...
        if (nextChar == FORWARD_SLASH_CHAR)
        {
            _bindingURL.setDestinationName(builder.toString());
            return BindingURLParserState.DESTINATION_SEPERATOR_CHAR;
        }
        // This is the case where destination is not explictly stated.
        // ex direct://amq.direct/myQueue?option1='1' ...
        else
        {
            _bindingURL.setQueueName(builder.toString());
            return BindingURLParserState.QUESTION_MARK_CHAR;
        }
    }

    private BindingURLParserState extractQueueName()
    {
        char nextChar = _url[_index];
        StringBuilder builder = new StringBuilder();
        while (nextChar != QUESTION_MARK_CHAR && nextChar != END_OF_URL_MARKER_CHAR)
        {
            builder.append(nextChar);
            nextChar = _url[++_index];
        }
        _bindingURL.setQueueName(builder.toString());

        if(nextChar == QUESTION_MARK_CHAR)
        {
            return BindingURLParserState.QUESTION_MARK_CHAR;
        }
        else
        {
            return BindingURLParserState.BINDING_URL_END;
        }
    }

    private BindingURLParserState extractPropertyName()
    {
        StringBuilder builder = new StringBuilder();
        char next = _url[_index];
        while (next != PROPERTY_EQUALS_CHAR)
        {
            builder.append(next);
            next = _url[++_index];
        }
        _currentPropName = builder.toString();

        if (_currentPropName.trim().equals(""))
        {
            _error = "Property name cannot be empty";
            return BindingURLParserState.ERROR;
        }

        return BindingURLParserState.PROPERTY_EQUALS;
    }

    private BindingURLParserState extractPropertyValue()
    {
        StringBuilder builder = new StringBuilder();
        char next = _url[_index];
        while (next != SINGLE_QUOTE_CHAR)
        {
            builder.append(next);
            next = _url[++_index];
        }
        String propValue = builder.toString();

        if (propValue.trim().equals(""))
        {
            _error = "Property values cannot be empty";
            return BindingURLParserState.ERROR;
        }
        else
        {
            if (_options.containsKey(_currentPropName))
            {
                Object obj = _options.get(_currentPropName);
                if (obj instanceof List)
                {
                    List list = (List)obj;
                    list.add(propValue);
                }
                else // it has to be a string
                {
                    List<String> list = new ArrayList();
                    list.add((String)obj);
                    list.add(propValue);
                    _options.put(_currentPropName, list);
                }
            }
            else
            {
                _options.put(_currentPropName, propValue);
            }


            return BindingURLParserState.END_PROPERTY_VALUE;
        }
    }

    private BindingURLParserState checkEndOfURL()
    {
        char nextChar = _url[_index];
        if ( nextChar ==  END_OF_URL_MARKER_CHAR)
        {
            return BindingURLParserState.BINDING_URL_END;
        }
        else if (nextChar == PROPERTY_SEPARATOR_CHAR || nextChar == ALTERNATIVE_PROPERTY_SEPARATOR_CHAR)
        {
            return BindingURLParserState.PROPERTY_SEPARATOR;
        }
        else
        {
            return BindingURLParserState.ERROR;
        }
    }

    private String markErrorLocation()
    {
        String tmp = String.valueOf(_url);
        // length -1 to remove ENDOF URL marker
        return tmp.substring(0,_index) + "^" + tmp.substring(_index+1> tmp.length()-1?tmp.length()-1:_index+1,tmp.length()-1);
    }

    private void processOptions() throws URISyntaxException
    {
//      check for bindingKey
        if (_options.containsKey(BindingURL.OPTION_BINDING_KEY) && _options.get(BindingURL.OPTION_BINDING_KEY) != null)
        {
            Object obj = _options.get(BindingURL.OPTION_BINDING_KEY);

            if (obj instanceof String)
            {
                AMQShortString[] bindingKeys = new AMQShortString[]{new AMQShortString((String)obj)};
                _bindingURL.setBindingKeys(bindingKeys);
            }
            else // it would be a list
            {
                List list = (List)obj;
                AMQShortString[] bindingKeys = new AMQShortString[list.size()];
                int i=0;
                for (Iterator it = list.iterator(); it.hasNext();)
                {
                    bindingKeys[i] = new AMQShortString((String)it.next());
                    i++;
                }
                _bindingURL.setBindingKeys(bindingKeys);
            }

        }
        for (String key: _options.keySet())
        {
            // We want to skip the bindingKey list
            if (_options.get(key) instanceof String)
            {
                _bindingURL.setOption(key, (String)_options.get(key));
            }
        }


        // check if both a binding key and a routing key is specified.
        if (_options.containsKey(BindingURL.OPTION_BINDING_KEY) && _options.containsKey(BindingURL.OPTION_ROUTING_KEY))
        {
            throw new URISyntaxException(String.valueOf(_url),"It is illegal to specify both a routingKey and a bindingKey in the same URL",-1);
        }

        // check for durable subscriptions
        if (_bindingURL.getExchangeClass().equals(ExchangeDefaults.TOPIC_EXCHANGE_CLASS))
        {
            String queueName = null;
            if (Boolean.parseBoolean(_bindingURL.getOption(BindingURL.OPTION_DURABLE)))
            {
                if (_bindingURL.containsOption(BindingURL.OPTION_CLIENTID) && _bindingURL.containsOption(BindingURL.OPTION_SUBSCRIPTION))
                {
                    queueName = _bindingURL.getOption(BindingURL.OPTION_CLIENTID) + ":" + _bindingURL.getOption(BindingURL.OPTION_SUBSCRIPTION);
                }
                else
                {
                    throw new URISyntaxException(String.valueOf(_url),"Durable subscription must have values for " + BindingURL.OPTION_CLIENTID
                        + " and " + BindingURL.OPTION_SUBSCRIPTION , -1);

                }
            }
            _bindingURL.setQueueName(queueName);
        }
    }

    public static void main(String[] args)
    {
        String[] urls = new String[]
           {
             "topic://amq.topic//myTopic?routingkey='stocks.#'",
             "topic://amq.topic/message_queue?bindingkey='usa.*'&bindingkey='control',exclusive='true'",
             "topic://amq.topic//?bindingKey='usa.*',bindingkey='control',exclusive='true'",
             "direct://amq.direct/dummyDest/myQueue?routingkey='abc.*'",
             "exchange.Class://exchangeName/Destination/Queue",
             "exchangeClass://exchangeName/Destination/?option='value',option2='value2'",
             "IBMPerfQueue1?durable='true'",
             "exchangeClass://exchangeName/Destination/?bindingkey='key1',bindingkey='key2'",
             "exchangeClass://exchangeName/Destination/?bindingkey='key1'&routingkey='key2'"
           };

        try
        {
            for (String url: urls)
            {
                System.out.println("URL " + url);
                AMQBindingURL bindingURL = new AMQBindingURL(url);
                BindingURLParser parser = new BindingURLParser(url,bindingURL);
                System.out.println("\nX " + bindingURL.toString() + " \n");

            }

        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }

}
