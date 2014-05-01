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
using System.Net;
using System.Text;
using System.Text.RegularExpressions;
using log4net;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Client
{

    public class URLHelper
    {
        public static char DEFAULT_OPTION_SEPERATOR = '&';
        public static char ALTERNATIVE_OPTION_SEPARATOR = ',';
        public static char BROKER_SEPARATOR = ';';

        /// <summary>
        /// 
        /// </summary>
        /// <param name="optionMap"></param>
        /// <param name="options"></param>
        public static void parseOptions(IDictionary optionMap, string options)
        {
            //options looks like this
            //brokerlist='tcp://host:port?option='value',option='value';vm://:3/virtualpath?option='value'',failover='method?option='value',option='value''

            if (options == null || options.IndexOf('=') == -1)
            {
                return;
            }

            int optionIndex = options.IndexOf('=');

            String option = options.Substring(0, optionIndex);

            int length = options.Length;

            int nestedQuotes = 0;

            // to store index of final "'"
            int valueIndex = optionIndex;

            //Walk remainder of url.
            while (nestedQuotes > 0 || valueIndex < length)
            {
                valueIndex++;

                if (valueIndex >= length)
                {
                    break;
                }

                if (options[valueIndex] == '\'')
                {
                    if (valueIndex + 1 < options.Length)
                    {
                        if (options[valueIndex + 1] == DEFAULT_OPTION_SEPERATOR ||
                                options[valueIndex + 1] == ALTERNATIVE_OPTION_SEPARATOR ||
                                options[valueIndex + 1] == BROKER_SEPARATOR ||
                                options[valueIndex + 1] == '\'')
                        {
                            nestedQuotes--;
                            //                        System.out.println(
                            //                                options + "\n" + "-" + nestedQuotes + ":" + getPositionString(valueIndex - 2, 1));
                            if (nestedQuotes == 0)
                            {
                                //We've found the value of an option
                                break;
                            }
                        }
                        else
                        {
                            nestedQuotes++;
                            //                        System.out.println(
                            //                                options + "\n" + "+" + nestedQuotes + ":" + getPositionString(valueIndex - 2, 1));
                        }
                    }
                    else
                    {
                        // We are at the end of the string
                        // Check to see if we are corectly closing quotes
                        if (options[valueIndex] == '\'')
                        {
                            nestedQuotes--;
                        }

                        break;
                    }
                }
            }

            if (nestedQuotes != 0 || valueIndex < (optionIndex + 2))
            {
                int sepIndex = 0;

                //Try and identify illegal separator character
                if (nestedQuotes > 1)
                {
                    for (int i = 0; i < nestedQuotes; i++)
                    {
                        sepIndex = options.IndexOf('\'', sepIndex);
                        sepIndex++;
                    }
                }

                if (sepIndex >= options.Length || sepIndex == 0)
                {
                    parseError(valueIndex, "Unterminated option", options);
                }
                else
                {
                    parseError(sepIndex, "Unterminated option. Possible illegal option separator:'" +
                            options[sepIndex] + "'", options);
                }
            }

            // optionIndex +2 to skip "='"
            int sublen = valueIndex - (optionIndex + 2);
            String value = options.Substring(optionIndex + 2, sublen);

            optionMap.Add(option, value);

            if (valueIndex < (options.Length - 1))
            {
                //Recurse to get remaining options
                parseOptions(optionMap, options.Substring(valueIndex + 2));
            }
        }


        public static void parseError(int index, String error, String url)
        {
            parseError(index, 1, error, url);
        }

        public static void parseError(int index, int length, String error, String url)
        {
            throw new UrlSyntaxException(url, error, index, length);
        }

        public static String printOptions(Hashtable options)
        {
            if (options.Count == 0)
            {
                return "";
            }
            else
            {
                StringBuilder sb = new StringBuilder();
                sb.Append('?');
                foreach (String key in options.Keys)
                {
                    sb.AppendFormat("{0}='{1}'{2}", key, options[key], DEFAULT_OPTION_SEPERATOR);
                }

                sb.Remove(sb.Length - 1, 1);
                return sb.ToString();
            }
        }

    }

    public class QpidConnectionUrl
    {
        internal static IConnectionInfo FromUrl(string fullURL)
        {
            //_url = fullURL;
            IConnectionInfo connectionInfo = new QpidConnectionInfo();


            //            _options = new HashMap<String, String>();
            //            _brokers = new LinkedList();
            //            _failoverOptions = new HashMap<String, String>();

            // Connection URL format
            //amqp://[user:pass@][clientid]/virtualhost?brokerlist='tcp://host:port?option=\'value\',option=\'value\';vm://:3/virtualpath?option=\'value\'',failover='method?option=\'value\',option='value''"
            // Options are of course optional except for requiring a single broker in the broker list.
            try
            {
                Uri connection = new Uri(fullURL);

                if (connection.Scheme == null || !(connection.Scheme.Equals(ConnectionUrlConstants.AMQ_PROTOCOL)))
                {
                    throw new UrlSyntaxException(fullURL, "Not an AMQP URL");
                }

                if (connection.Host != null && connection.Host.Length > 0 && !connection.Host.Equals("default"))
                {
                    connectionInfo.ClientName = connection.Host;
                }

                String userInfo = connection.UserInfo;
                if (userInfo == null || userInfo.Length == 0)
                {
                    URLHelper.parseError(ConnectionUrlConstants.AMQ_PROTOCOL.Length + 3,
                            "User information not found on url", fullURL);
                }
                else
                {
                    parseUserInfo(userInfo, fullURL, connectionInfo);
                }
                String virtualHost = connection.AbsolutePath; // XXX: is AbsolutePath corrrect?

                if (virtualHost != null && virtualHost.Length > 0)
                {
                    connectionInfo.VirtualHost = virtualHost;
                }
                else
                {
                    int authLength = connection.Authority.Length;
                    int start = ConnectionUrlConstants.AMQ_PROTOCOL.Length + 3;
                    int testIndex = start + authLength;
                    if (testIndex < fullURL.Length && fullURL[testIndex] == '?')
                    {
                        URLHelper.parseError(start, testIndex - start, "Virtual host found", fullURL);
                    }
                    else
                    {
                        URLHelper.parseError(-1, "Virtual host not specified", fullURL);
                    }

                }

                QpidConnectionInfo qci = (QpidConnectionInfo)connectionInfo;
                string query = connection.Query;
                if (query[0] == '?') query = query.Substring(1);
                URLHelper.parseOptions(qci.GetOptions(), query);

                processOptions(connectionInfo);

                //Fragment is #string (not used)
                //System.out.println(connection.getFragment());
                return connectionInfo;
            }
            catch (UriFormatException uris)
            {
                throw uris;
                //                if (uris is UrlSyntaxException)
                //                {
                //                    throw uris;
                //                }
                //
                //                int slash = fullURL.IndexOf("\\");
                //
                //                if (slash == -1)
                //                {
                //                    URLHelper.parseError(uris.GetIndex(), uris.getReason(), uris.getInput());
                //                }
                //                else
                //                {
                //                    if (slash != 0 && fullURL.charAt(slash - 1) == ':')
                //                    {
                //                        URLHelper.parseError(slash - 2, fullURL.indexOf('?') - slash + 2, "Virtual host looks like a windows path, forward slash not allowed in URL", fullURL);
                //                    }
                //                    else
                //                    {
                //                        URLHelper.parseError(slash, "Forward slash not allowed in URL", fullURL);
                //                    }
                //                }
            }
        }

        private static void parseUserInfo(String userinfo, string fullUrl, IConnectionInfo connectionInfo)
        {
            //user info = user:pass

            int colonIndex = userinfo.IndexOf(':');

            if (colonIndex == -1)
            {
                URLHelper.parseError(ConnectionUrlConstants.AMQ_PROTOCOL.Length + 3,
                    userinfo.Length, "Null password in user information not allowed.", fullUrl);
            }
            else
            {
                connectionInfo.Username = userinfo.Substring(0, colonIndex);
                connectionInfo.Password = userinfo.Substring(colonIndex + 1);
            }
        }

        private static void processOptions(IConnectionInfo connectionInfo)
        {
            string brokerlist = connectionInfo.GetOption(ConnectionUrlConstants.OPTIONS_BROKERLIST);
            if (brokerlist != null)
            {
                //brokerlist tcp://host:port?option='value',option='value';vm://:3/virtualpath?option='value'
                Regex splitter = new Regex("" + URLHelper.BROKER_SEPARATOR);

                foreach (string broker in splitter.Split(brokerlist))
                {
                    connectionInfo.AddBrokerInfo(new AmqBrokerInfo(broker));
                }

                connectionInfo.SetOption(ConnectionUrlConstants.OPTIONS_BROKERLIST, null);
                //                _options.remove(OPTIONS_BROKERLIST);
            }

            string failover = connectionInfo.GetOption(ConnectionUrlConstants.OPTIONS_FAILOVER);
            if (failover != null)
            {
                // failover='method?option='value',option='value''

                int methodIndex = failover.IndexOf('?');

                if (methodIndex > -1)
                {
                    connectionInfo.FailoverMethod = failover.Substring(0, methodIndex);
                    QpidConnectionInfo qpidConnectionInfo = (QpidConnectionInfo)connectionInfo;
                    URLHelper.parseOptions(qpidConnectionInfo.GetFailoverOptions(),
                        failover.Substring(methodIndex + 1));
                }
                else
                {
                    connectionInfo.FailoverMethod = failover;
                }

                connectionInfo.SetOption(ConnectionUrlConstants.OPTIONS_FAILOVER, null);
                //                _options.remove(OPTIONS_FAILOVER);
            }
        }

        internal static IConnectionInfo FromUri(Uri uri)
        {
            return null; // FIXME

        }
    }

    public class QpidConnectionInfo : IConnectionInfo
    {
        const string DEFAULT_VHOST = "/";
        string _username = "guest";
        string _password = "guest";
        string _virtualHost = DEFAULT_VHOST;

        string _failoverMethod = null;
        IDictionary _failoverOptions = new Hashtable();
        IDictionary _options = new Hashtable();
        IList _brokerInfos = new ArrayList(); // List<BrokerInfo>
        string _clientName = String.Format("{0}{1:G}", Dns.GetHostName(), DateTime.Now.Ticks);

        public IDictionary GetFailoverOptions()
        {
            return _failoverOptions;
        }

        public IDictionary GetOptions()
        {
            return _options;
        }

        public static IConnectionInfo FromUrl(String url)
        {
            return QpidConnectionUrl.FromUrl(url);
        }

        public string AsUrl()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendFormat("{0}://", ConnectionUrlConstants.AMQ_PROTOCOL);

            if (_username != null)
            {
                sb.Append(_username);
                if (_password != null)
                {
                    sb.AppendFormat(":{0}", _password);
                }
                sb.Append("@");
            }

            sb.Append(_clientName);
            sb.Append(_virtualHost);
            sb.Append(OptionsToString());

            return sb.ToString();
        }

        private String OptionsToString()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendFormat("?{0}='", ConnectionUrlConstants.OPTIONS_BROKERLIST);

            foreach (IBrokerInfo broker in _brokerInfos)
            {
                sb.AppendFormat("{0};", broker);
            }

            sb.Remove(sb.Length - 1, 1);
            sb.Append("'");

            if (_failoverMethod != null)
            {
                sb.AppendFormat("{0}{1}='{2}{3}'", URLHelper.DEFAULT_OPTION_SEPERATOR,
                    ConnectionUrlConstants.OPTIONS_FAILOVER,
                    _failoverMethod,
                    URLHelper.printOptions((Hashtable)_failoverOptions));
            }

            return sb.ToString();
        }


        public string FailoverMethod
        {
            get { return _failoverMethod; }
            set { _failoverMethod = value; }
        }

        public string GetFailoverOption(string key)
        {
            return (string)_failoverOptions[key];
        }

        public int BrokerCount
        {
            get { return _brokerInfos.Count; }
        }

        public IBrokerInfo GetBrokerInfo(int index)
        {
            return (IBrokerInfo)_brokerInfos[index];
        }

        public void AddBrokerInfo(IBrokerInfo brokerInfo)
        {
            if (!_brokerInfos.Contains(brokerInfo))
            {
                _brokerInfos.Add(brokerInfo);
            }
        }

        public IList GetAllBrokerInfos()
        {
            return _brokerInfos;
        }

        public string ClientName
        {
            get { return _clientName; }
            set { _clientName = value; }
        }

        public string Username
        {
            get { return _username; }
            set { _username = value; }
        }

        public string Password
        {
            get { return _password; }
            set { _password = value; }
        }

        public string VirtualHost
        {
            get { return _virtualHost; }
            set { 
               _virtualHost = value;
               if ( _virtualHost == null || _virtualHost.Length == 0 )
                  _virtualHost = DEFAULT_VHOST;
               if ( _virtualHost[0] != '/' )
                  _virtualHost = '/' + _virtualHost;
            }
        }

        public string GetOption(string key)
        {
            return (string)_options[key];
        }

        public void SetOption(string key, string value)
        {
            _options[key] = value;
        }

        public override string ToString()
        {
            return AsUrl();
        }
    }
}
