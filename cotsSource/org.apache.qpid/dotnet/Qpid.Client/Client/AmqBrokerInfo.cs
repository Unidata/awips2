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
using System;
using System.Collections;
using System.Text;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Client
{
    public class AmqBrokerInfo : IBrokerInfo
    {
        public readonly string URL_FORMAT_EXAMPLE =
            "<transport>://<hostname>[:<port Default=\""+BrokerInfoConstants.DEFAULT_PORT+"\">][?<option>='<value>'[,<option>='<value>']]";

        public const long DEFAULT_CONNECT_TIMEOUT = 30000L;

        private string _host = "localhost";
        private int _port = 5672;
        private string _transport = "amqp";
        private Hashtable _options = new Hashtable();
        private SslOptions _sslOptions;

        public AmqBrokerInfo()
        {
        }

        public AmqBrokerInfo(string url)
        {
            // URL should be of format tcp://host:port?option='value',option='value'
            try
            {
                Uri connection = new Uri(url);

                String transport = connection.Scheme;

                // Handles some defaults to minimise changes to existing broker URLS e.g. localhost
                if (transport != null)
                {
                    transport = transport.ToLower();
                    //todo this list of valid transports should be enumerated somewhere
                    if ((!(transport.Equals("vm") || transport.Equals("tcp"))))
                    {
                        if (transport.Equals("localhost"))
                        {
                            connection = new Uri(BrokerInfoConstants.DEFAULT_TRANSPORT + "://" + url);
                            transport = connection.Scheme;
                        }
                        else
                        {
                            if (url[transport.Length] == ':' && url[transport.Length + 1] != '/')
                            {
                                //Then most likely we have a host:port value
                                connection = new Uri(BrokerInfoConstants.DEFAULT_TRANSPORT + "://" + url);
                                transport = connection.Scheme;
                            }
                            else
                            {
                                URLHelper.parseError(0, transport.Length, "Unknown transport", url);
                            }
                        }
                    }
                }
                else
                {
                    //Default the transport
                    connection = new Uri(BrokerInfoConstants.DEFAULT_TRANSPORT + "://" + url);
                    transport = connection.Scheme;
                }

                if (transport == null)
                {
                    URLHelper.parseError(-1, "Unknown transport:'" + transport + "'" +
                                             " In broker URL:'" + url + "' Format: " + URL_FORMAT_EXAMPLE, "");
                }

                Transport = transport;

                String host = connection.Host;
                if (!host.Equals("default")) Host = host;

                int port = connection.Port;

                if (port == -1)
                {
                    // Fix for when there is port data but it is not automatically parseable by getPort().
                    String auth = connection.Authority;

                    if (auth != null && auth.IndexOf(':') != -1)
                    {
                        int start = auth.IndexOf(":") + 1;
                        int end = start;
                        bool looking = true;
                        bool found = false;
                        //Walk the authority looking for a port value.
                        while (looking)
                        {
                            try
                            {
                                end++;
                                int.Parse(auth.Substring(start, end-start+1));

                                if (end >= auth.Length)
                                {
                                    looking = false;
                                    found = true;
                                }
                            }
                            catch (FormatException)
                            {
                                looking = false;
                            }

                        }
                        if (found)
                        {
                            Port = int.Parse(auth.Substring(start, end-start+1));
                        }
                        else
                        {
                            URLHelper.parseError(connection.ToString().IndexOf(connection.Authority) + end - 1,
                                                 "Illegal character in port number", connection.ToString());
                        }
                    }
                    else
                    {
                        Port = BrokerInfoConstants.DEFAULT_PORT;
                    }
                }
                else
                {
                    Port = port;
                }

                String queryString = connection.Query;
                if (queryString.Length > 0 && queryString[0] == '?')
                {
                    queryString = queryString.Substring(1);
                }

                URLHelper.parseOptions(_options, queryString);

                //Fragment is #string (not used)
            }
            catch (UriFormatException uris)
            {
                throw uris;
//                if (uris is UrlSyntaxException)
//                {
//                    throw uris;
//                }
//
//                URLHelper.parseError(uris.getIndex(), uris.getReason(), uris.getInput());
            }
        }

        public AmqBrokerInfo(string transport, string host, int port, bool useSSL) : this()
        {
            _transport = transport;
            _host = host;
            _port = port;

            if (useSSL)
            {
                SetOption(BrokerInfoConstants.OPTIONS_SSL, "true");
            }
        }

        public AmqBrokerInfo(string transport, string host, int port, SslOptions sslConfig)
           : this()
        {
           _transport = transport;
           _host = host;
           _port = port;
            
           if ( sslConfig != null )
           {
              SetOption(BrokerInfoConstants.OPTIONS_SSL, "true");
              _sslOptions = sslConfig;
           }
        }


        public string Host
        {
            get { return _host; }
            set { _host = value; }
        }

        public int Port
        {
            get { return _port; }
            set { _port = value; }
        }

        public string Transport
        {
            get { return _transport; }
            set { _transport = value; }
        }

        public SslOptions SslOptions
        {
            get { return _sslOptions; }
        }

        public string GetOption(string key)
        {
            return (string)_options[key];
        }

        public void SetOption(string key, string value)
        {
            _options[key] = value;
        }

        public long Timeout
        {
            get
            {
                if ( _options.ContainsKey(BrokerInfoConstants.OPTIONS_CONNECT_TIMEOUT) )
                {
                    try
                    {
                        return long.Parse(GetOption(BrokerInfoConstants.OPTIONS_CONNECT_TIMEOUT));
                    } catch ( FormatException )
                    {
                        //Do nothing as we will use the default below.
                    }
                }
                return BrokerInfoConstants.DEFAULT_CONNECT_TIMEOUT;
            }
            set
            {
                SetOption(BrokerInfoConstants.OPTIONS_CONNECT_TIMEOUT, value.ToString());
            }
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();

            sb.Append(_transport);
            sb.Append("://");

            if (!(StringEqualsIgnoreCase(_transport, "vm")))
            {
                sb.Append(_host);
            }

            sb.Append(':');
            sb.Append(_port);

            sb.Append(URLHelper.printOptions(_options));

            return sb.ToString();
        }
        
		public override bool Equals(object obj)
		{
	        if (!(obj is IBrokerInfo))
	        {
	            return false;
	        }
	
	        IBrokerInfo bd = (IBrokerInfo) obj;
	        return StringEqualsIgnoreCase(_host, bd.Host) &&
	        	_port == bd.Port &&
                StringEqualsIgnoreCase(_transport, bd.Transport) &&
                UseSSL == bd.UseSSL;
        }
    	
		public override int GetHashCode()
		{
			return _host.ToLower().GetHashCode() ^ _port.GetHashCode();
		}

        // TODO: move to util class.
        private bool StringEqualsIgnoreCase(string one, string two)
        {
            return one.ToLower().Equals(two.ToLower());
        }

        public bool UseSSL
        {
            get
            {
                // To be friendly to users we should be case insensitive.
                // or simply force users to conform to OPTIONS_SSL
                // todo make case insensitive by trying ssl Ssl sSl ssL SSl SsL sSL SSL

                if ( _options.ContainsKey(BrokerInfoConstants.OPTIONS_SSL) )
                {
                    return StringEqualsIgnoreCase(GetOption(BrokerInfoConstants.OPTIONS_SSL), "true");
                }

                return false;
            }
            set
            {
                SetOption(BrokerInfoConstants.OPTIONS_SSL, value.ToString());
            }
        }
    }
}
