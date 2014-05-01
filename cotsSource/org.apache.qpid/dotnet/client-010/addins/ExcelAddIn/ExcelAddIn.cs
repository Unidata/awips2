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
using System.Collections.Generic;
using System.Configuration;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.Office.Interop.Excel;
using org.apache.qpid.client;
using org.apache.qpid.transport;

namespace ExcelAddIn
{
    public delegate string ProcessMessage(IMessage m);

    /// <summary>   
    /// This interface must be implemented so to use a user defined message processor 
    /// </summary>
    public interface MessageProcessor
    {
        string ProcessMessage(IMessage m);
    }

    [ComVisible(true), ProgId("Qpid")]
    public class ExcelAddIn : IRtdServer 
    {
        private IRTDUpdateEvent _onMessage;
        private readonly Dictionary<int, IMessage> _topicMessages = new Dictionary<int, IMessage>();
        private readonly Dictionary<string, QpidListener> _queueListener = new Dictionary<string, QpidListener>();
        private readonly Dictionary<int, string> _topicQueueName = new Dictionary<int, string>();
        private IClient _client;
        private IClientSession _session;
        private ProcessMessage _messageProcessor;

        #region properties

        public IRTDUpdateEvent OnMessage
        {
            get { return _onMessage; }
        }

        public Dictionary<int, IMessage>  TopicMessages
        {
            get { return _topicMessages; }
        }

        public IClientSession Session
        {
            get { return _session; }
        }

        #endregion


        #region IRtdServer Members

        /// <summary>
        /// Called when Excel requests the first RTD topic for the server. 
        /// Connect to the broker, returns a on success and 0 otherwise
        /// </summary>
        /// <param name="CallbackObject"></param>
        /// <returns></returns>
        public int ServerStart(IRTDUpdateEvent CallbackObject)
        {
            _onMessage = CallbackObject;  
            string host = "localhost";
            string port = "5673";
            string virtualhost = "test";
            string username = "guest";
            string password = "guest";
            _messageProcessor = getMessage;
          
            if( ConfigurationManager.AppSettings["Host"] != null )
            {
                host = ConfigurationManager.AppSettings["Host"];
            }
            if (ConfigurationManager.AppSettings["Port"] != null)
            {
                port = ConfigurationManager.AppSettings["Port"];
            }
            if (ConfigurationManager.AppSettings["VirtualHost"] != null)
            {
                virtualhost = ConfigurationManager.AppSettings["VirtualHost"];
            }
            if (ConfigurationManager.AppSettings["Username"] != null)
            {
                username = ConfigurationManager.AppSettings["UserName"];
            }
            if (ConfigurationManager.AppSettings["Password"] != null)
            {
                password = ConfigurationManager.AppSettings["Password"];
            }
            if (ConfigurationManager.AppSettings["ProcessorAssembly"] != null)
            {
                try
                {
                    Assembly a = Assembly.LoadFrom(ConfigurationManager.AppSettings["ProcessorAssembly"]);
                    Object o = a.CreateInstance(ConfigurationManager.AppSettings["ProcessorClass"]);
                    MessageProcessor p = (MessageProcessor) o;
                    _messageProcessor = p.ProcessMessage;
                }
                catch (Exception e)
                {
                    System.Windows.Forms.MessageBox.Show("Error: \n" + e.StackTrace);         
                    return 0;
                }
            }

            System.Windows.Forms.MessageBox.Show("Connection parameters: \n host: " + host + "\n port: " 
                                                 + port + "\n user: " + username);
            try
            {
                _client = new Client();            
                _client.Connect(host, Convert.ToInt16(port), virtualhost, username, password);
                // create a session 
                _session = _client.CreateSession(0);          
            }
            catch (Exception e)
            {
                System.Windows.Forms.MessageBox.Show("Error: \n" + e.StackTrace);         
                return 0;
            }
            
            // always successful 
            return 1;
        }

        /// <summary>
        /// Called whenever Excel requests a new RTD topic from the RealTimeData server.
        /// </summary>
        /// <param name="TopicID"></param>
        /// <param name="Strings"></param>
        /// <param name="GetNewValues"></param>
        /// <returns></returns>
        public object ConnectData(int TopicID, ref Array Strings, ref bool GetNewValues)
        {
            try
            {
                string queuename = "defaultExcelAddInQueue";
                string destinationName = "ExcelAddIn-" + queuename;
                if( Strings.Length > 0 )
                {
                    queuename = (string) Strings.GetValue(0);
                }
                // Error message if the queue does not exist
                QueueQueryResult result = (QueueQueryResult)_session.QueueQuery(queuename).Result;
                if( result.GetQueue() == null )
                {
                    System.Windows.Forms.MessageBox.Show("Error: \n queue " + queuename + " does not exist");
                    return "error";  
                }
          
                QpidListener listener;
                _topicMessages.Add(TopicID, null);
                _topicQueueName.Add(TopicID, queuename);
                if (_queueListener.ContainsKey(queuename))
                {
                    listener = _queueListener[queuename];
                    listener.addTopic(TopicID);
                }
                else
                {
                    listener = new QpidListener(this);
                    listener.addTopic(TopicID);
                    _queueListener.Add(queuename, listener);
                    _session.AttachMessageListener(listener, destinationName);
                    _session.MessageSubscribe(queuename, destinationName, MessageAcceptMode.EXPLICIT,
                                              MessageAcquireMode.PRE_ACQUIRED, null, 0, null);
                    // issue credits     
                    _session.MessageSetFlowMode(destinationName, MessageFlowMode.WINDOW);
                    _session.MessageFlow(destinationName, MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);
                    _session.MessageFlow(destinationName, MessageCreditUnit.MESSAGE, 1000);
                    _session.Sync();
                }                
            }
            catch (Exception e)
            {
                System.Windows.Forms.MessageBox.Show("Error: \n" + e.StackTrace);
                return "error";
            }
            return "waiting";
        }

        /// <summary>
        /// Called whenever Excel no longer requires a specific topic.
        /// </summary>
        /// <param name="TopicID"></param>
        public void DisconnectData(int TopicID)
        {
            _topicMessages.Remove(TopicID);
            string queueName = _topicQueueName[TopicID];
            if (_topicQueueName.Remove(TopicID) && !_topicQueueName.ContainsValue(queueName))
            {
                _session.MessageStop("ExcelAddIn-" + queueName);
                _session.MessageListeners.Remove("ExcelAddIn-" + queueName);
            }
        }

        public int Heartbeat()
        {
            return 1;
        }

        public Array RefreshData(ref int TopicCount)
        {
            Array result = new object[2, _topicMessages.Count];
            foreach (KeyValuePair<int, IMessage> pair in _topicMessages)
            {
                result.SetValue(pair.Key, 0, pair.Key);
                string value = _messageProcessor(pair.Value);
                result.SetValue(value, 1, pair.Key);                
            }
            TopicCount = _topicMessages.Count; 
            return result;
        }

        public void ServerTerminate()
        {
            
        }

        #endregion
        //END IRTDServer METHODS

        private string getMessage(IMessage m)
        {
            string res;
            BinaryReader reader = new BinaryReader(m.Body, Encoding.UTF8);
            byte[] body = new byte[m.Body.Length - m.Body.Position];
            reader.Read(body, 0, body.Length);
            ASCIIEncoding enc = new ASCIIEncoding();            
            res = enc.GetString(body);           
            return res;
        }

    }

    class QpidListener : IMessageListener
    {
        private readonly ExcelAddIn _excel;       
        private readonly List<int> _topics = new List<int>();

        public QpidListener(ExcelAddIn excel)
        {
            _excel = excel;
        }

        public void addTopic(int topic)
        {
            _topics.Add(topic);
        }

        public void MessageTransfer(IMessage m)
        {            
            foreach (int i in _topics)
            {
                if (_excel.TopicMessages.ContainsKey(i))
                {
                    _excel.TopicMessages[i] = m;
                }
            }
            // ack this message 
            RangeSet rs = new RangeSet();
            rs.Add(m.Id);
            _excel.Session.MessageAccept(rs);        
            _excel.OnMessage.UpdateNotify();
        }
    }
}
