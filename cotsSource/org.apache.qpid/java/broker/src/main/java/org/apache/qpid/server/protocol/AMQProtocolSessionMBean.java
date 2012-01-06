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
/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.apache.qpid.server.protocol;

import java.util.Date;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.monitor.MonitorNotification;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ConnectionCloseBody;
import org.apache.qpid.framing.MethodRegistry;
import org.apache.qpid.management.common.mbeans.ManagedConnection;
import org.apache.qpid.management.common.mbeans.annotations.MBeanConstructor;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.actors.ManagementActor;
import org.apache.qpid.server.logging.LogActor;
import org.apache.qpid.server.logging.RootMessageLogger;
import org.apache.qpid.server.management.AMQManagedObject;
import org.apache.qpid.server.management.ManagedObject;

/**
 * This MBean class implements the management interface. In order to make more attributes, operations and notifications
 * available over JMX simply augment the ManagedConnection interface and add the appropriate implementation here.
 */
@MBeanDescription("Management Bean for an AMQ Broker Connection")
public class AMQProtocolSessionMBean extends AMQManagedObject implements ManagedConnection
{
    private AMQProtocolSession _protocolSession = null;
    private String _name = null;

    // openmbean data types for representing the channel attributes

    private static final OpenType[] _channelAttributeTypes =
        { SimpleType.INTEGER, SimpleType.BOOLEAN, SimpleType.STRING, SimpleType.INTEGER, SimpleType.BOOLEAN };
    private static CompositeType _channelType = null; // represents the data type for channel data
    private static TabularType _channelsType = null; // Data type for list of channels type
    private static final AMQShortString BROKER_MANAGEMENT_CONSOLE_HAS_CLOSED_THE_CONNECTION =
        new AMQShortString("Broker Management Console has closed the connection.");

    @MBeanConstructor("Creates an MBean exposing an AMQ Broker Connection")
    public AMQProtocolSessionMBean(AMQProtocolSession amqProtocolSession) throws NotCompliantMBeanException, OpenDataException
    {
        super(ManagedConnection.class, ManagedConnection.TYPE, ManagedConnection.VERSION);
        _protocolSession = amqProtocolSession;
        String remote = getRemoteAddress();
        remote = "anonymous".equals(remote) ? (remote + hashCode()) : remote;
        _name = jmxEncode(new StringBuffer(remote), 0).toString();
        init();
    }

    static
    {
        try
        {
            init();
        }
        catch (JMException ex)
        {
            // This is not expected to ever occur.
            throw new RuntimeException("Got JMException in static initializer.", ex);
        }
    }

    /**
     * initialises the openmbean data types
     */
    private static void init() throws OpenDataException
    {
        _channelType =
            new CompositeType("Channel", "Channel Details", COMPOSITE_ITEM_NAMES, COMPOSITE_ITEM_DESCRIPTIONS,
                _channelAttributeTypes);
        _channelsType = new TabularType("Channels", "Channels", _channelType, TABULAR_UNIQUE_INDEX);
    }

    public String getClientId()
    {
        return (_protocolSession.getContextKey() == null) ? null : _protocolSession.getContextKey().toString();
    }

    public String getAuthorizedId()
    {
        return (_protocolSession.getPrincipal() != null ) ? _protocolSession.getPrincipal().getName() : null;
    }

    public String getVersion()
    {
        return (_protocolSession.getClientVersion() == null) ? null : _protocolSession.getClientVersion().toString();
    }

    public Date getLastIoTime()
    {
        return new Date(_protocolSession.getLastIoTime());
    }

    public String getRemoteAddress()
    {
        return _protocolSession.getRemoteAddress().toString();
    }

    public ManagedObject getParentObject()
    {
        return _protocolSession.getVirtualHost().getManagedObject();
    }

    public Long getWrittenBytes()
    {
        return _protocolSession.getWrittenBytes();
    }

    public Long getReadBytes()
    {
        return _protocolSession.getWrittenBytes();
    }

    public Long getMaximumNumberOfChannels()
    {
        return _protocolSession.getMaximumNumberOfChannels();
    }

    public void setMaximumNumberOfChannels(Long value)
    {
        _protocolSession.setMaximumNumberOfChannels(value);
    }

    public String getObjectInstanceName()
    {
        return _name;
    }

    /**
     * commits transactions for a transactional channel
     *
     * @param channelId
     * @throws JMException if channel with given id doesn't exist or if commit fails
     */
    public void commitTransactions(int channelId) throws JMException
    {
        CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        try
        {
            AMQChannel channel = _protocolSession.getChannel(channelId);
            if (channel == null)
            {
                throw new JMException("The channel (channel Id = " + channelId + ") does not exist");
            }

            _protocolSession.commitTransactions(channel);
        }
        catch (AMQException ex)
        {
            throw new MBeanException(ex, ex.toString());
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    /**
     * rollsback the transactions for a transactional channel
     *
     * @param channelId
     * @throws JMException if channel with given id doesn't exist or if rollback fails
     */
    public void rollbackTransactions(int channelId) throws JMException
    {
        CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        try
        {
            AMQChannel channel = _protocolSession.getChannel(channelId);
            if (channel == null)
            {
                throw new JMException("The channel (channel Id = " + channelId + ") does not exist");
            }

            _protocolSession.rollbackTransactions(channel);
        }
        catch (AMQException ex)
        {
            throw new MBeanException(ex, ex.toString());
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    /**
     * Creates the list of channels in tabular form from the _channelMap.
     *
     * @return list of channels in tabular form.
     * @throws OpenDataException
     */
    public TabularData channels() throws OpenDataException
    {
        TabularDataSupport channelsList = new TabularDataSupport(_channelsType);
        List<AMQChannel> list = _protocolSession.getChannels();

        for (AMQChannel channel : list)
        {
            Object[] itemValues =
                {
                    channel.getChannelId(), channel.isTransactional(),
                    (channel.getDefaultQueue() != null) ? channel.getDefaultQueue().getName().asString() : null,
                    channel.getUnacknowledgedMessageMap().size(), channel.getBlocking()
                };

            CompositeData channelData = new CompositeDataSupport(_channelType, COMPOSITE_ITEM_NAMES, itemValues);
            channelsList.put(channelData);
        }

        return channelsList;
    }

    /**
     * closes the connection. The administrator can use this management operation to close connection to free up
     * resources.
     * @throws JMException
     */
    public void closeConnection() throws JMException
    {

        MethodRegistry methodRegistry = _protocolSession.getMethodRegistry();
        ConnectionCloseBody responseBody =
                methodRegistry.createConnectionCloseBody(AMQConstant.REPLY_SUCCESS.getCode(),
                                                         // replyCode
                                                         BROKER_MANAGEMENT_CONSOLE_HAS_CLOSED_THE_CONNECTION,
                                                         // replyText,
                                                         0,
                                                         0);

        // This seems ugly but because we use closeConnection in both normal
        // broker operation and as part of the management interface it cannot
        // be avoided. The Current Actor will be null when this method is
        // called via the Management interface. This is because we allow the
        // Local API connection with JConsole. If we did not allow that option
        // then the CurrentActor could be set in our JMX Proxy object.
        // As it is we need to set the CurrentActor on all MBean methods
        // Ideally we would not have a single method that can be called from
        // two contexts.        
        boolean removeActor = false;
        if (CurrentActor.get() == null)
        {
            removeActor = true;
            CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        }

        try
        {
            _protocolSession.writeFrame(responseBody.generateFrame(0));

            try
            {

                _protocolSession.closeSession();
            }
            catch (AMQException ex)
            {
                throw new MBeanException(ex, ex.toString());
            }
        }
        finally
        {
            if (removeActor)
            {
                CurrentActor.remove();
            }
        }
    }

    @Override
    public MBeanNotificationInfo[] getNotificationInfo()
    {
        String[] notificationTypes = new String[] { MonitorNotification.THRESHOLD_VALUE_EXCEEDED };
        String name = MonitorNotification.class.getName();
        String description = "Channel count has reached threshold value";
        MBeanNotificationInfo info1 = new MBeanNotificationInfo(notificationTypes, name, description);

        return new MBeanNotificationInfo[] { info1 };
    }

    public void notifyClients(String notificationMsg)
    {
        Notification n =
            new Notification(MonitorNotification.THRESHOLD_VALUE_EXCEEDED, this, ++_notificationSequenceNumber,
                System.currentTimeMillis(), notificationMsg);
        _broadcaster.sendNotification(n);
    }

} // End of MBean class
