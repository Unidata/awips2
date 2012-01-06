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
package org.apache.qpid.server.queue;

import org.apache.log4j.Logger;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.management.common.mbeans.ManagedQueue;
import org.apache.qpid.management.common.mbeans.annotations.MBeanConstructor;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.server.management.AMQManagedObject;
import org.apache.qpid.server.management.ManagedObject;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.server.message.AMQMessageHeader;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.message.MessageTransferMessage;
import org.apache.qpid.server.txn.ServerTransaction;
import org.apache.qpid.server.txn.LocalTransaction;
import org.apache.qpid.transport.MessageProperties;

import javax.management.JMException;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.OperationsException;
import javax.management.monitor.MonitorNotification;
import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import java.text.SimpleDateFormat;
import java.util.*;

/**
 * AMQQueueMBean is the management bean for an {@link AMQQueue}.
 *
 * <p/><tablse id="crc"><caption>CRC Caption</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * </table>
 */
@MBeanDescription("Management Interface for AMQQueue")
public class AMQQueueMBean extends AMQManagedObject implements ManagedQueue, QueueNotificationListener
{
    /** Used for debugging purposes. */
    private static final Logger _logger = Logger.getLogger(AMQQueueMBean.class);

    private static final SimpleDateFormat _dateFormat = new SimpleDateFormat("MM-dd-yy HH:mm:ss.SSS z");

    private AMQQueue _queue = null;
    private String _queueName = null;
    // OpenMBean data types for viewMessages method

    private static OpenType[] _msgAttributeTypes = new OpenType[5]; // AMQ message attribute types.
    private static CompositeType _messageDataType = null; // Composite type for representing AMQ Message data.
    private static TabularType _messagelistDataType = null; // Datatype for representing AMQ messages list.

    // OpenMBean data types for viewMessageContent method
    private static CompositeType _msgContentType = null;
    private static OpenType[] _msgContentAttributeTypes = new OpenType[4];

    private final long[] _lastNotificationTimes = new long[NotificationCheck.values().length];
    private Notification _lastNotification = null;




    @MBeanConstructor("Creates an MBean exposing an AMQQueue")
    public AMQQueueMBean(AMQQueue queue) throws JMException
    {
        super(ManagedQueue.class, ManagedQueue.TYPE, ManagedQueue.VERSION);
        _queue = queue;
        _queueName = jmxEncode(new StringBuffer(queue.getName()), 0).toString();
    }

    public ManagedObject getParentObject()
    {
        return _queue.getVirtualHost().getManagedObject();
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
        _msgContentAttributeTypes[0] = SimpleType.LONG; // For message id
        _msgContentAttributeTypes[1] = SimpleType.STRING; // For MimeType
        _msgContentAttributeTypes[2] = SimpleType.STRING; // For Encoding
        _msgContentAttributeTypes[3] = new ArrayType(1, SimpleType.BYTE); // For message content
        _msgContentType = new CompositeType("Message Content", "AMQ Message Content",
                    VIEW_MSG_CONTENT_COMPOSITE_ITEM_NAMES, VIEW_MSG_CONTENT_COMPOSITE_ITEM_DESCRIPTIONS,
                    _msgContentAttributeTypes);

        _msgAttributeTypes[0] = SimpleType.LONG; // For message id
        _msgAttributeTypes[1] = new ArrayType(1, SimpleType.STRING); // For header attributes
        _msgAttributeTypes[2] = SimpleType.LONG; // For size
        _msgAttributeTypes[3] = SimpleType.BOOLEAN; // For redelivered
        _msgAttributeTypes[4] = SimpleType.LONG; // For queue position

        _messageDataType = new CompositeType("Message", "AMQ Message", VIEW_MSGS_COMPOSITE_ITEM_NAMES,
                                VIEW_MSGS_COMPOSITE_ITEM_DESCRIPTIONS, _msgAttributeTypes);
        _messagelistDataType = new TabularType("Messages", "List of messages", _messageDataType,
                                                VIEW_MSGS_TABULAR_UNIQUE_INDEX);
    }

    public String getObjectInstanceName()
    {
        return _queueName;
    }

    public String getName()
    {
        return _queueName;
    }

    public boolean isDurable()
    {
        return _queue.isDurable();
    }

    public String getOwner()
    {
        return String.valueOf(_queue.getPrincipalHolder() == null
                              ? null
                              : _queue.getPrincipalHolder().getPrincipal() == null
                                ? null
                                : _queue.getPrincipalHolder().getPrincipal().getName());
    }

    public boolean isAutoDelete()
    {
        return _queue.isAutoDelete();
    }

    public Integer getMessageCount()
    {
        return _queue.getMessageCount();
    }

    public Long getMaximumMessageSize()
    {
        return _queue.getMaximumMessageSize();
    }

    public Long getMaximumMessageAge()
    {
        return _queue.getMaximumMessageAge();
    }

    public void setMaximumMessageAge(Long maximumMessageAge)
    {
        _queue.setMaximumMessageAge(maximumMessageAge);
    }

    public void setMaximumMessageSize(Long value)
    {
        _queue.setMaximumMessageSize(value);
    }

    public Integer getConsumerCount()
    {
        return _queue.getConsumerCount();
    }

    public Integer getActiveConsumerCount()
    {
        return _queue.getActiveConsumerCount();
    }

    public Long getReceivedMessageCount()
    {
        return _queue.getReceivedMessageCount();
    }

    public Long getMaximumMessageCount()
    {
        return _queue.getMaximumMessageCount();
    }

    public void setMaximumMessageCount(Long value)
    {
        _queue.setMaximumMessageCount(value);
    }

    /**
     * returns the maximum total size of messages(bytes) in the queue.
     */
    public Long getMaximumQueueDepth()
    {
        return _queue.getMaximumQueueDepth();
    }

    public void setMaximumQueueDepth(Long value)
    {
        _queue.setMaximumQueueDepth(value);
    }

    /**
     * returns the total size of messages(bytes) in the queue.
     */
    public Long getQueueDepth() throws JMException
    {
        return _queue.getQueueDepth();
    }

    public Long getCapacity()
    {
        return _queue.getCapacity();
    }

    public void setCapacity(Long capacity) throws IllegalArgumentException
    {
        if( _queue.getFlowResumeCapacity() > capacity )
        {
            throw new IllegalArgumentException("Capacity must not be less than FlowResumeCapacity");
        }
        
    	_queue.setCapacity(capacity);
    }

    public Long getFlowResumeCapacity()
    {
        return _queue.getFlowResumeCapacity();
    }

    public void setFlowResumeCapacity(Long flowResumeCapacity) throws IllegalArgumentException
    {
        if( _queue.getCapacity() < flowResumeCapacity )
        {
            throw new IllegalArgumentException("FlowResumeCapacity must not exceed Capacity");
        }
        
        _queue.setFlowResumeCapacity(flowResumeCapacity);
    }
    
    public boolean isFlowOverfull()
    {
        return _queue.isOverfull();
    }

    /**
     * Checks if there is any notification to be send to the listeners
     */
    public void checkForNotification(ServerMessage msg) throws AMQException
    {

        final Set<NotificationCheck> notificationChecks = _queue.getNotificationChecks();

        if(!notificationChecks.isEmpty())
        {
            final long currentTime = System.currentTimeMillis();
            final long thresholdTime = currentTime - _queue.getMinimumAlertRepeatGap();

            for (NotificationCheck check : notificationChecks)
            {
                if (check.isMessageSpecific() || (_lastNotificationTimes[check.ordinal()] < thresholdTime))
                {
                    if (check.notifyIfNecessary(msg, _queue, this))
                    {
                        _lastNotificationTimes[check.ordinal()] = currentTime;
                    }
                }
            }
        }

    }

    /**
     * Sends the notification to the listeners
     */
    public void notifyClients(NotificationCheck notification, AMQQueue queue, String notificationMsg)
    {
        // important : add log to the log file - monitoring tools may be looking for this
        _logger.info(notification.name() + " On Queue " + queue.getName() + " - " + notificationMsg);
        notificationMsg = notification.name() + " " + notificationMsg;

        _lastNotification =
            new Notification(MonitorNotification.THRESHOLD_VALUE_EXCEEDED, this, ++_notificationSequenceNumber,
                System.currentTimeMillis(), notificationMsg);

        _broadcaster.sendNotification(_lastNotification);
    }

    public Notification getLastNotification()
    {
        return _lastNotification;
    }

    /**
     * @see AMQQueue#deleteMessageFromTop
     */
    public void deleteMessageFromTop() throws JMException
    {
        _queue.deleteMessageFromTop();
    }

    /**
     * Clears the queue of non-acquired messages
     *
     * @return the number of messages deleted
     * @see AMQQueue#clearQueue
     */
    public Long clearQueue() throws JMException
    {
        return _queue.clearQueue();
    }

    /**
     * returns message content as byte array and related attributes for the given message id.
     */
    public CompositeData viewMessageContent(long msgId) throws JMException
    {
        QueueEntry entry = _queue.getMessageOnTheQueue(msgId);

        if (entry == null)
        {
            throw new OperationsException("AMQMessage with message id = " + msgId + " is not in the " + _queueName);
        }

        ServerMessage serverMsg = entry.getMessage();
        final int bodySize = (int) serverMsg.getSize();


        List<Byte> msgContent = new ArrayList<Byte>();

        java.nio.ByteBuffer buf = java.nio.ByteBuffer.allocate(bodySize);
        int position = 0;

        while(position < bodySize)
        {
            position += serverMsg.getContent(buf, position);
            buf.flip();
            for(int i = 0; i < buf.limit(); i++)
            {
                msgContent.add(buf.get(i));
            }
            buf.clear();
        }

        AMQMessageHeader header = serverMsg.getMessageHeader();

        String mimeType = null, encoding = null;
        if (header != null)
        {
            mimeType = header.getMimeType();

            encoding = header.getEncoding();
        }


        Object[] itemValues = { msgId, mimeType, encoding, msgContent.toArray(new Byte[0]) };

        return new CompositeDataSupport(_msgContentType, VIEW_MSG_CONTENT_COMPOSITE_ITEM_NAMES, itemValues);

    }

    /**
     * Returns the header contents of the messages stored in this queue in tabular form.
     * Deprecated as of Qpid JMX API 1.3
     */
    @Deprecated
    public TabularData viewMessages(int beginIndex, int endIndex) throws JMException
    {
        return viewMessages((long)beginIndex,(long)endIndex);
    }


    /**
     * Returns the header contents of the messages stored in this queue in tabular form.
     * @param startPosition The queue position of the first message to be viewed
     * @param endPosition The queue position of the last message to be viewed
     */
    public TabularData viewMessages(long startPosition, long endPosition) throws JMException
    {
        if ((startPosition > endPosition) || (startPosition < 1))
        {
            throw new OperationsException("From Index = " + startPosition + ", To Index = " + endPosition
                + "\n\"From Index\" should be greater than 0 and less than \"To Index\"");
        }

        if ((endPosition - startPosition) > Integer.MAX_VALUE)
        {
            throw new OperationsException("Specified MessageID interval is too large. Intervals must be less than 2^31 in size");
        }

        List<QueueEntry> list = _queue.getMessagesRangeOnTheQueue(startPosition,endPosition);
        TabularDataSupport _messageList = new TabularDataSupport(_messagelistDataType);

        try
        {
            // Create the tabular list of message header contents
            int size = list.size();

            for (int i = 0; i < size ; i++)
            {
                long position = startPosition + i;
                final QueueEntry queueEntry = list.get(i);
                ServerMessage serverMsg = queueEntry.getMessage();
                if(serverMsg instanceof AMQMessage)
                {
                    AMQMessage msg = (AMQMessage) serverMsg;
                    ContentHeaderBody headerBody = msg.getContentHeaderBody();
                    // Create header attributes list
                    String[] headerAttributes = getMessageHeaderProperties(headerBody);
                    Object[] itemValues = {msg.getMessageId(), headerAttributes, headerBody.bodySize, queueEntry.isRedelivered(), position};
                    CompositeData messageData = new CompositeDataSupport(_messageDataType, VIEW_MSGS_COMPOSITE_ITEM_NAMES, itemValues);
                    _messageList.put(messageData);

                }
                else if(serverMsg instanceof MessageTransferMessage)
                {
                    // We have a 0-10 message
                    MessageTransferMessage msg = (MessageTransferMessage) serverMsg;

                    // Create header attributes list
                    String[] headerAttributes = getMessageTransferMessageHeaderProps(msg);
                    Object[] itemValues = {msg.getMessageNumber(), headerAttributes, msg.getSize(), queueEntry.isRedelivered(), position};
                    CompositeData messageData = new CompositeDataSupport(_messageDataType, VIEW_MSGS_COMPOSITE_ITEM_NAMES, itemValues);
                    _messageList.put(messageData);
                }
                else
                {
                    //unknown message
                    String[] headerAttributes = new String[]{"N/A"};
                    Object[] itemValues = { serverMsg.getMessageNumber(), headerAttributes, serverMsg.getSize(), queueEntry.isRedelivered(), position};
                    CompositeData messageData = new CompositeDataSupport(_messageDataType, VIEW_MSGS_COMPOSITE_ITEM_NAMES, itemValues);
                    _messageList.put(messageData);
                }
            }
        }
        catch (AMQException e)
        {
            JMException jme = new JMException("Error creating message contents: " + e);
            jme.initCause(e);
            throw jme;
        }

        return _messageList;
    }

    private String[] getMessageHeaderProperties(ContentHeaderBody headerBody)
    {
        List<String> list = new ArrayList<String>();
        BasicContentHeaderProperties headerProperties = (BasicContentHeaderProperties) headerBody.properties;
        list.add("reply-to = " + headerProperties.getReplyToAsString());
        list.add("propertyFlags = " + headerProperties.getPropertyFlags());
        list.add("ApplicationID = " + headerProperties.getAppIdAsString());
        list.add("ClusterID = " + headerProperties.getClusterIdAsString());
        list.add("UserId = " + headerProperties.getUserIdAsString());
        list.add("JMSMessageID = " + headerProperties.getMessageIdAsString());
        list.add("JMSCorrelationID = " + headerProperties.getCorrelationIdAsString());

        int delMode = headerProperties.getDeliveryMode();
        list.add("JMSDeliveryMode = " +
                ((delMode == BasicContentHeaderProperties.PERSISTENT) ? "Persistent" : "Non_Persistent"));

        list.add("JMSPriority = " + headerProperties.getPriority());
        list.add("JMSType = " + headerProperties.getType());

        long longDate = headerProperties.getExpiration();
        String strDate = (longDate != 0) ? _dateFormat.format(new Date(longDate)) : null;
        list.add("JMSExpiration = " + strDate);

        longDate = headerProperties.getTimestamp();
        strDate = (longDate != 0) ? _dateFormat.format(new Date(longDate)) : null;
        list.add("JMSTimestamp = " + strDate);

        return list.toArray(new String[list.size()]);
    }

    private String[] getMessageTransferMessageHeaderProps(MessageTransferMessage msg)
    {
        List<String> list = new ArrayList<String>();
        
        AMQMessageHeader header = msg.getMessageHeader();
        MessageProperties msgProps = msg.getHeader().get(MessageProperties.class);

        String appID = null;
        String userID = null;

        if(msgProps != null)
        {
            appID = msgProps.getAppId() == null ? "null" : new String(msgProps.getAppId());
            userID = msgProps.getUserId() == null ? "null" : new String(msgProps.getUserId());
        }

        list.add("reply-to = " + header.getReplyTo());
        list.add("propertyFlags = "); //TODO
        list.add("ApplicationID = " + appID);
        list.add("ClusterID = "); //TODO
        list.add("UserId = " + userID);
        list.add("JMSMessageID = " + header.getMessageId());
        list.add("JMSCorrelationID = " + header.getCorrelationId());
        list.add("JMSDeliveryMode = " + (msg.isPersistent() ? "Persistent" : "Non_Persistent"));
        list.add("JMSPriority = " + header.getPriority());
        list.add("JMSType = " + header.getType());

        long longDate = header.getExpiration();
        String strDate = (longDate != 0) ? _dateFormat.format(new Date(longDate)) : null;
        list.add("JMSExpiration = " + strDate);

        longDate = header.getTimestamp();
        strDate = (longDate != 0) ? _dateFormat.format(new Date(longDate)) : null;
        list.add("JMSTimestamp = " + strDate);

        return list.toArray(new String[list.size()]);
    }

    /**
     * @see ManagedQueue#moveMessages
     * @param fromMessageId
     * @param toMessageId
     * @param toQueueName
     * @throws JMException
     */
    public void moveMessages(long fromMessageId, long toMessageId, String toQueueName) throws JMException
    {
        if ((fromMessageId > toMessageId) || (fromMessageId < 1))
        {
            throw new OperationsException("\"From MessageId\" should be greater than 0 and less than \"To MessageId\"");
        }

        ServerTransaction txn = new LocalTransaction(_queue.getVirtualHost().getTransactionLog());
        _queue.moveMessagesToAnotherQueue(fromMessageId, toMessageId, toQueueName, txn);
        txn.commit();
    }

    /**
     * @see ManagedQueue#deleteMessages
     * @param fromMessageId
     * @param toMessageId
     * @throws JMException
     */
    public void deleteMessages(long fromMessageId, long toMessageId) throws JMException
    {
        if ((fromMessageId > toMessageId) || (fromMessageId < 1))
        {
            throw new OperationsException("\"From MessageId\" should be greater than 0 and less than \"To MessageId\"");
        }

        _queue.removeMessagesFromQueue(fromMessageId, toMessageId);
    }

    /**
     * @see ManagedQueue#copyMessages
     * @param fromMessageId
     * @param toMessageId
     * @param toQueueName
     * @throws JMException
     */
    public void copyMessages(long fromMessageId, long toMessageId, String toQueueName) throws JMException
    {
        if ((fromMessageId > toMessageId) || (fromMessageId < 1))
        {
            throw new OperationsException("\"From MessageId\" should be greater than 0 and less than \"To MessageId\"");
        }

        ServerTransaction txn = new LocalTransaction(_queue.getVirtualHost().getTransactionLog());

        _queue.copyMessagesToAnotherQueue(fromMessageId, toMessageId, toQueueName, txn);

        txn.commit();


    }

    /**
     * returns Notifications sent by this MBean.
     */
    @Override
    public MBeanNotificationInfo[] getNotificationInfo()
    {
        String[] notificationTypes = new String[] { MonitorNotification.THRESHOLD_VALUE_EXCEEDED };
        String name = MonitorNotification.class.getName();
        String description = "Either Message count or Queue depth or Message size has reached threshold high value";
        MBeanNotificationInfo info1 = new MBeanNotificationInfo(notificationTypes, name, description);

        return new MBeanNotificationInfo[] { info1 };
    }

} // End of AMQQueueMBean class
