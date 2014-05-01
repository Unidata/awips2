/* Licensed to the Apache Software Foundation (ASF) under one
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
package org.apache.qpid.client.url;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.qpid.client.AMQBrokerDetails;
import org.apache.qpid.jms.BrokerDetails;

/**
 * The format Qpid URL is based on the AMQP one.
 * The grammar is as follows:
 * <p> qpid_url          = "qpid:" [client_props "@"] port_addr_list ["/" future-parameters]
 * <p> port_addr_list 	 = [port_addr ","]* port_addr
 * <p> port_addr         = tcp_port_addr | tls_prot_addr | future_prot_addr
 * <p> tcp_port_addr     = tcp_id tcp_addr
 * <p> tcp_id            = "tcp:" | ""
 * <p> tcp_addr          = host [":" port]
 * <p> host              = <as per http://www.apps.ietf.org/>
 * <p> port              = number
 * <p> tls_prot_addr     = tls_id tls_addr
 * <p> tls_id            = "tls:" | ""
 * <p> tls_addr          = host [":" port]
 * <p> future_prot_addr  = future_prot_id future_prot_addr
 * <p> future_prot_id    = <placeholder, must end in ":". Example "sctp:">
 * <p> future_prot_addr  = <placeholder, protocl-specific address>
 * <p> future_parameters = <placeholder, not used in failover addresses>
 * <p> client_props      = [client_prop ";"]*  client_prop
 * <p> client_prop       = prop "=" val
 * <p> prop              = chars as per <as per http://www.apps.ietf.org/>
 * <p> val               = valid as per <as per http://www.apps.ietf.org/>
 * <p/>
 * Ex: qpid:virtualhost=tcp:host-foo,test,client_id=foo@tcp:myhost.com:5672,virtualhost=prod;
 * keystore=/opt/keystore@client_id2@tls:mysecurehost.com:5672
 */
public class URLParser_0_10
{
    private static final char[] URL_START_SEQ = new char[]{'q', 'p', 'i', 'd', ':'};
    private static final char PROPERTY_EQUALS_CHAR = '=';
    private static final char PROPERTY_SEPARATOR_CHAR = ';';
    private static final char ADDRESS_SEPERATOR_CHAR = ',';

    //private static final char CLIENT_ID_TRANSPORT_SEPARATOR_CHAR = ':';
    private static final char TRANSPORT_HOST_SEPARATOR_CHAR = ':';
    private static final char HOST_PORT_SEPARATOR_CHAR = ':';
    private static final char AT_CHAR = '@';
    private static final char END_OF_URL_MARKER = '^';

    enum URLParserState
    {
        QPID_URL_START,
        ADDRESS_START,
        PROPERTY_NAME,
        PROPERTY_EQUALS,
        PROPERTY_VALUE,
        PROPERTY_SEPARATOR,
        AT_CHAR,
        TRANSPORT,
        TRANSPORT_HOST_SEPARATOR,
        HOST,
        HOST_PORT_SEPARATOR,
        PORT,
        ADDRESS_END,
        ADDRESS_SEPERATOR,
        QPID_URL_END,
        ERROR
    }

    //-- Constructors

    private char[] _url;
    private List<BrokerDetails> _brokerDetailList = new ArrayList<BrokerDetails>();
    private String _error;
    private int _index = 0;
    private BrokerDetails _currentBroker;
    private String _currentPropName;
    private boolean _endOfURL = false;
    private URLParserState _currentParserState;

    public URLParser_0_10(String url) throws MalformedURLException
    {
        _url = (url + END_OF_URL_MARKER).toCharArray();
        _endOfURL = false;
        _currentParserState = URLParserState.QPID_URL_START;
        URLParserState prevState = _currentParserState; // for error handling
        try
        {
            while (_currentParserState != URLParserState.ERROR && _currentParserState != URLParserState.QPID_URL_END)
            {
                prevState = _currentParserState;
                _currentParserState = next();
            }

            if (_currentParserState == URLParserState.ERROR)
            {
                _error =
                        "Invalid URL format [current_state = " + prevState + ", broker details parsed so far " + _currentBroker + " ] error at (" + _index + ") due to " + _error;
                MalformedURLException ex;
                ex = new MalformedURLException(_error);
                throw ex;
            }
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            _error = "Invalid URL format [current_state = " + prevState + ", broker details parsed so far " + _currentBroker + " ] error at (" + _index + ")";
            MalformedURLException ex = new MalformedURLException(_error);
            throw ex;
        }
    }

    //-- interface QpidURL
    public List<BrokerDetails> getAllBrokerDetails()
    {
        return _brokerDetailList;
    }

    public String getURL()
    {
        return new String(_url);
    }

    private URLParserState next()
    {
        switch (_currentParserState)
        {
            case QPID_URL_START:
                return checkSequence(URL_START_SEQ, URLParserState.ADDRESS_START);
            case ADDRESS_START:
                return startAddress();
            case PROPERTY_NAME:
                return extractPropertyName();
            case PROPERTY_EQUALS:
                _index++; // skip the equal sign
                return URLParserState.PROPERTY_VALUE;
            case PROPERTY_VALUE:
                return extractPropertyValue();
            case PROPERTY_SEPARATOR:
                _index++; // skip ","
                return URLParserState.PROPERTY_NAME;
            case AT_CHAR:
                _index++; // skip the @ sign
                return URLParserState.TRANSPORT;
            case TRANSPORT:
                return extractTransport();
            case TRANSPORT_HOST_SEPARATOR:
                _index++; // skip ":"
                return URLParserState.HOST;
            case HOST:
                return extractHost();
            case HOST_PORT_SEPARATOR:
                _index++; // skip ":"
                return URLParserState.PORT;
            case PORT:
                return extractPort();
            case ADDRESS_END:
                return endAddress();
            case ADDRESS_SEPERATOR:
                _index++; // skip ","
                return URLParserState.ADDRESS_START;
            default:
                return URLParserState.ERROR;
        }
    }

    private URLParserState checkSequence(char[] expected, URLParserState nextPart)
    {
        for (char expectedChar : expected)
        {
            if (expectedChar != _url[_index])
            {
                _error = "Excepted (" + expectedChar + ") at position " + _index + ", got (" + _url[_index] + ")";
                return URLParserState.ERROR;
            }
            _index++;
        }
        return nextPart;
    }

    private URLParserState startAddress()
    {
        _currentBroker = new AMQBrokerDetails();

        for (int j = _index; j < _url.length; j++)
        {
            if (_url[j] == PROPERTY_EQUALS_CHAR)
            {
                return URLParserState.PROPERTY_NAME;
            }
            else if (_url[j] == ADDRESS_SEPERATOR_CHAR)
            {
                return URLParserState.TRANSPORT;
            }
        }
        return URLParserState.TRANSPORT;
    }

    private URLParserState endAddress()
    {
        _brokerDetailList.add(_currentBroker);
        if (_endOfURL)
        {
            return URLParserState.QPID_URL_END;
        }
        else
        {
            return URLParserState.ADDRESS_SEPERATOR;
        }
    }

    private URLParserState extractPropertyName()
    {
        StringBuilder b = new StringBuilder();
        char next = _url[_index];
        while (next != PROPERTY_EQUALS_CHAR && next != AT_CHAR)
        {
            b.append(next);
            next = _url[++_index];
        }
        _currentPropName = b.toString();
        if (_currentPropName.trim().equals(""))
        {
            _error = "Property name cannot be empty";
            return URLParserState.ERROR;
        }
        else if (next == PROPERTY_EQUALS_CHAR)
        {
            return URLParserState.PROPERTY_EQUALS;
        }
        else
        {
            return URLParserState.AT_CHAR;
        }
    }

    private URLParserState extractPropertyValue()
    {
        StringBuilder b = new StringBuilder();
        char next = _url[_index];
        while (next != PROPERTY_SEPARATOR_CHAR && next != AT_CHAR)
        {
            b.append(next);
            next = _url[++_index];
        }
        String propValue = b.toString();
        if (propValue.trim().equals(""))
        {
            _error = "Property values cannot be empty";
            return URLParserState.ERROR;
        }
        else
        {
            _currentBroker.setProperty(_currentPropName, propValue);
            if (next == PROPERTY_SEPARATOR_CHAR)
            {
                return URLParserState.PROPERTY_SEPARATOR;
            }
            else
            {
                return URLParserState.AT_CHAR;
            }
        }
    }

    private URLParserState extractTransport()
    {
        String transport = buildUntil(TRANSPORT_HOST_SEPARATOR_CHAR);
        if (transport.trim().equals(""))
        {
            _error = "Transport cannot be empty";
            return URLParserState.ERROR;
        }
        else if (!(transport.trim().equals(BrokerDetails.PROTOCOL_TCP) || transport.trim()
                .equals(BrokerDetails.PROTOCOL_TLS)))
        {
            _error = "Transport cannot be " + transport + " value must be tcp or tls";
            return URLParserState.ERROR;
        }
        else
        {
            _currentBroker.setTransport(transport);
            return URLParserState.TRANSPORT_HOST_SEPARATOR;
        }
    }

    private URLParserState extractHost()
    {
        char nextSep = 'c';
        String host;
        URLParserState nextState;

        for (int i = _index; i < _url.length; i++)
        {
            if (_url[i] == HOST_PORT_SEPARATOR_CHAR)
            {
                nextSep = HOST_PORT_SEPARATOR_CHAR;
                break;
            }
            else if (_url[i] == ADDRESS_SEPERATOR_CHAR)
            {
                nextSep = ADDRESS_SEPERATOR_CHAR;
                break;
            }
        }

        if (nextSep == HOST_PORT_SEPARATOR_CHAR)
        {
            host = buildUntil(HOST_PORT_SEPARATOR_CHAR);
            nextState = URLParserState.HOST_PORT_SEPARATOR;
        }
        else if (nextSep == ADDRESS_SEPERATOR_CHAR)
        {
            host = buildUntil(ADDRESS_SEPERATOR_CHAR);
            nextState = URLParserState.ADDRESS_END;
        }
        else
        {
            host = buildUntil(END_OF_URL_MARKER);
            nextState = URLParserState.ADDRESS_END;
            _endOfURL = true;
        }

        if (host.trim().equals(""))
        {
            _error = "Host cannot be empty";
            return URLParserState.ERROR;
        }
        else
        {
            _currentBroker.setHost(host);
            return nextState;
        }
    }


    private URLParserState extractPort()
    {

        StringBuilder b = new StringBuilder();
        try
        {
            char next = _url[_index];
            while (next != ADDRESS_SEPERATOR_CHAR && next != END_OF_URL_MARKER )
            {
                b.append(next);
                next = _url[++_index];
            }
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            _endOfURL = true;
        }
        String portStr = b.toString();
        if (portStr.trim().equals(""))
        {
            _error = "Host cannot be empty";
            return URLParserState.ERROR;
        }
        else
        {
            try
            {
                int port = Integer.parseInt(portStr);
                _currentBroker.setPort(port);
                if( _url[_index] == END_OF_URL_MARKER )
                {
                    _endOfURL = true;
                }
                return URLParserState.ADDRESS_END;
            }
            catch (NumberFormatException e)
            {
                _error = "Illegal number for port";
                return URLParserState.ERROR;
            }
        }
    }

    private String buildUntil(char c)
    {
        StringBuilder b = new StringBuilder();
        char next = _url[_index];
        while (next != c)
        {
            b.append(next);
            next = _url[++_index];
        }
        return b.toString();
    }

    public static void main(String[] args)
    {
        String testurl = "qpid:password=pass;username=name@tcp:test1";
        try
        {
            URLParser_0_10 impl = new URLParser_0_10(testurl);
            for (BrokerDetails d : impl.getAllBrokerDetails())
            {
                System.out.println(d);
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}
