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
package org.apache.qpid.jms.failover;

import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.Session;

import org.apache.qpid.client.AMQAnyDestination;
import org.apache.qpid.client.AMQBrokerDetails;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ConnectionURL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * When using the Failover exchange a single broker is supplied in the URL.
 * The connection will then connect to the cluster using the above broker details.
 * Once connected, the membership details of the cluster will be obtained via 
 * subscribing to a queue bound to the failover exchange.
 * 
 * The failover exchange will provide a list of broker URLs in the format "transport:ip:port"
 * Out of this list we only select brokers that match the transport of the original 
 * broker supplied in the connection URL.
 * 
 * Also properties defined for the original broker will be applied to all the brokers selected
 * from the list.   
 */

public class FailoverExchangeMethod implements FailoverMethod, MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(FailoverExchangeMethod.class);
   
    /** This is not safe to use until attainConnection is called */
    private AMQConnection _conn;
    
    /** Protects the broker list when modifications happens */
    private Object _brokerListLock = new Object();
    
    /** The session used to subscribe to failover exchange */
    private Session _ssn;
    
    private BrokerDetails _originalBrokerDetail;
    
    /** The index into the hostDetails array of the broker to which we are connected */
    private int _currentBrokerIndex = 0;
    
    /** The broker currently selected **/
    private BrokerDetails _currentBrokerDetail;
        
    /** Array of BrokerDetail used to make connections. */
    private ConnectionURL _connectionDetails;
    
    /** Denotes the number of failed attempts **/
    private int _failedAttemps = 0;
    
    public FailoverExchangeMethod(ConnectionURL connectionDetails, AMQConnection conn)
    {
        _connectionDetails = connectionDetails;
        _originalBrokerDetail = _connectionDetails.getBrokerDetails(0);
        
        // This is not safe to use until attainConnection is called, as this ref will not initialized fully.
        // The reason being this constructor is called inside the AMWConnection constructor.
        // It would be best if we find a way to pass this ref after AMQConnection is fully initialized.
        _conn = conn;
    }

    private void subscribeForUpdates() throws JMSException
    {
        if (_ssn == null)
        {
            _ssn = _conn.createSession(false,Session.AUTO_ACKNOWLEDGE);
            MessageConsumer cons = _ssn.createConsumer(
                                        new AMQAnyDestination(new AMQShortString("amq.failover"),
                                                              new AMQShortString("amq.failover"),
                                                              new AMQShortString(""),
                                                              true,true,null,false,
                                                              new AMQShortString[0])); 
            cons.setMessageListener(this);
        }                               
    }
    
    public void onMessage(Message m)
    {
        _logger.info("Failover exchange notified cluster membership change");
        
        String currentBrokerIP = ""; 
        try
        {
            currentBrokerIP = InetAddress.getByName(_currentBrokerDetail.getHost()).getHostAddress();
        }
        catch(Exception e)
        {
            _logger.warn("Unable to resolve current broker host name",e);
        }
        
        List<BrokerDetails> brokerList = new ArrayList<BrokerDetails>();
        try
        {            
            List<String> list = (List<String>)m.getObjectProperty("amq.failover");
            for (String brokerEntry:list)
            {                
                String[] urls = brokerEntry.substring(5) .split(",");
                // Iterate until you find the correct transport
                // Need to reconsider the logic when the C++ broker supports
                // SSL URLs.
                for (String url:urls)
                {
                    String[] tokens = url.split(":");
                    if (tokens[0].equalsIgnoreCase(_originalBrokerDetail.getTransport()))
                    {
                        BrokerDetails broker = new AMQBrokerDetails();
                        broker.setTransport(tokens[0]);
                        broker.setHost(tokens[1]);
                        broker.setPort(Integer.parseInt(tokens[2]));
                        broker.setProperties(_originalBrokerDetail.getProperties());
                        broker.setSSLConfiguration(_originalBrokerDetail.getSSLConfiguration());
                        brokerList.add(broker);
                        
                        if (currentBrokerIP.equals(broker.getHost()) && 
                            _currentBrokerDetail.getPort() == broker.getPort())
                        {
                            _currentBrokerIndex = brokerList.indexOf(broker);
                        }
                        
                        break;
                    }
                }                
            }
        }
        catch(JMSException e)
        {
            _logger.error("Error parsing the message sent by failover exchange",e);
        }
        
        synchronized (_brokerListLock)
        {
            _connectionDetails.setBrokerDetails(brokerList);
        }
        
        _logger.info("============================================================");
        _logger.info("Updated cluster membership details " + _connectionDetails);
        _logger.info("============================================================");
    }
    
    public void attainedConnection()
    {
        try
        {
            _failedAttemps = 0;
            _logger.info("============================================================");
            _logger.info("Attained connection ");
            _logger.info("============================================================");
            subscribeForUpdates();
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Unable to subscribe for cluster membership updates",e);
        }
    }

    public BrokerDetails getCurrentBrokerDetails()
    {
        synchronized (_brokerListLock)
        {
            _currentBrokerDetail = _connectionDetails.getBrokerDetails(_currentBrokerIndex);
            return _currentBrokerDetail;
        }
    }   
    
    public BrokerDetails getNextBrokerDetails()
    {
        synchronized(_brokerListLock)
        {
            if (_currentBrokerIndex == (_connectionDetails.getBrokerCount() - 1))
            {
                _currentBrokerIndex = 0;
            }
            else
            {
                _currentBrokerIndex++;
            }
            
            BrokerDetails broker = _connectionDetails.getBrokerDetails(_currentBrokerIndex);
            
            // When the broker list is updated it will include the current broker as well
            // There is no point trying it again, so trying the next one.
            if (_currentBrokerDetail != null &&
                broker.getHost().equals(_currentBrokerDetail.getHost()) &&
                broker.getPort() == _currentBrokerDetail.getPort())
            {
                if (_connectionDetails.getBrokerCount() > 1)
                {
                    return getNextBrokerDetails();
                }
                else
                {
                    _failedAttemps ++;
                    return null;
                }
            }

            String delayStr = broker.getProperty(BrokerDetails.OPTIONS_CONNECT_DELAY);
            if (delayStr != null)
            {
                Long delay = Long.parseLong(delayStr);
                _logger.info("Delay between connect retries:" + delay);
                try
                {
                    Thread.sleep(delay);
                }
                catch (InterruptedException ie)
                {
                    return null;
                }
            }
            else
            {
                _logger.info("No delay between connect retries, use tcp://host:port?connectdelay='value' to enable.");
            }

            _failedAttemps ++;
            _currentBrokerDetail = broker;
            return broker;            
        }
    }
    
    public boolean failoverAllowed()
    {
        // We allow to Failover provided 
        // our broker list is not empty and
        // we haven't gone through all of them  
               
        boolean b = _connectionDetails.getBrokerCount() > 0 &&
               _failedAttemps <= _connectionDetails.getBrokerCount();
        
        
        _logger.info("============================================================");
        _logger.info(toString());
        _logger.info("FailoverAllowed " + b);
        _logger.info("============================================================");
        
        return b;
    }
    
    public void reset()
    {
        _failedAttemps = 0;
    }
    
    public void setBroker(BrokerDetails broker)
    {
        // not sure if this method is needed
    }

    public void setRetries(int maxRetries)
    {
        // no max retries we keep trying as long
        // as we get updates
    }

    public String methodName()
    {
        return "Failover Exchange";
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append("FailoverExchange:\n");
        sb.append("\n Current Broker Index:");
        sb.append(_currentBrokerIndex);
        sb.append("\n Failed Attempts:");
        sb.append(_failedAttemps);
        sb.append("\n Orignal broker details:");
        sb.append(_originalBrokerDetail).append("\n");
        sb.append("\n -------- Broker List -----------\n");
        for (int i = 0; i < _connectionDetails.getBrokerCount(); i++)
        {
            if (i == _currentBrokerIndex)
            {
                sb.append(">");
            }

            sb.append(_connectionDetails.getBrokerDetails(i));
            sb.append("\n");
        }
        sb.append("--------------------------------\n");
        return sb.toString();
    }
}
