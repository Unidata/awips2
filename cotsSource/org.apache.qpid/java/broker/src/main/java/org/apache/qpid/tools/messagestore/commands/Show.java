/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.tools.messagestore.commands;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.tools.messagestore.MessageStoreTool;
import org.apache.qpid.tools.utils.Console;

import java.util.LinkedList;
import java.util.List;

public class Show extends AbstractCommand
{
    protected boolean _amqHeaders = false;
    protected boolean _routing = false;
    protected boolean _msgHeaders = false;

    public Show(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Shows the messages headers.";
    }

    public String usage()
    {
        return getCommand() + " [show=[all],[msgheaders],[amqheaders],[routing]] [id=<msgid e.g. 1,2,4-10>]";
    }

    public String getCommand()
    {
        return "show";
    }

    public void execute(String... args)
    {
        assert args.length > 0;
        assert args[0].equals(getCommand());

        if (args.length < 2)
        {
            parseArgs("all");
        }
        else
        {
            parseArgs(args);
        }

        performShow();
    }

    protected void parseArgs(String... args)
    {
        List<Long> msgids = null;

        if (args.length >= 2)
        {
            for (String arg : args)
            {
                if (arg.startsWith("show="))
                {
                    _msgHeaders = arg.contains("msgheaders") || arg.contains("all");
                    _amqHeaders = arg.contains("amqheaders") || arg.contains("all");
                    _routing = arg.contains("routing") || arg.contains("all");
                }

                if (arg.startsWith("id="))
                {
                    _tool.getState().setMessages(msgids);
                }
            }//for args
        }// if args > 2
    }

    protected void performShow()
    {
        if (_tool.getState().getVhost() == null)
        {
            _console.println("No Virtualhost selected. 'DuSelect' a Virtualhost first.");
            return;
        }

        AMQQueue _queue = _tool.getState().getQueue();

        List<Long> msgids = _tool.getState().getMessages();

        if (_queue != null)
        {
            List<QueueEntry> messages = _queue.getMessagesOnTheQueue();
            if (messages == null || messages.size() == 0)
            {
                _console.println("No messages on queue");
                return;
            }

            List<List> data = createMessageData(msgids, messages, _amqHeaders, _routing, _msgHeaders);
            if (data != null)
            {
                _console.printMap(null, data);
            }
            else
            {
                String message = "No data to display.";
                if (msgids != null)
                {
                    message += " Is message selection correct? " + _tool.getState().printMessages();
                }
                _console.println(message);
            }

        }
        else
        {
            _console.println("No Queue specified to show.");
        }
    }

    /**
     * Create the list data for display from the messages.
     *
     * @param msgids The list of message ids to display
     * @param messages A list of messages to format and display.
     * @param showHeaders should the header info be shown
     * @param showRouting show the routing info be shown
     * @param showMessageHeaders show the msg headers be shown
     * @return the formated data lists for printing
     */
    protected List<List> createMessageData(List<Long> msgids, List<QueueEntry> messages, boolean showHeaders, boolean showRouting,
                                           boolean showMessageHeaders)
    {

        // Currenly exposed message properties
//        //Printing the content Body
//        msg.getContentBodyIterator();
//        //Print the Headers
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getAppId();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getAppIdAsString();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getClusterId();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getContentType();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getCorrelationId();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getDeliveryMode();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getEncoding();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getExpiration();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getHeaders();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getMessageNumber();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getPriority();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getPropertyFlags();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getReplyTo();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getTimestamp();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getType();
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getUserId();
//
//        //Print out all the property names
//        ((BasicContentHeaderProperties)msg.getContentHeaderBody().properties).getHeaders().getPropertyNames();
//
//        msg.getMessageNumber();
//        msg.getSize();
//        msg.getArrivalTime();

//        msg.getDeliveredSubscription();
//        msg.getDeliveredToConsumer();
//        msg.getMessageHandle();
//        msg.getMessageNumber();
//        msg.getMessagePublishInfo();
//        msg.getPublisher();

//        msg.getStoreContext();
//        msg.isAllContentReceived();
//        msg.isPersistent();
//        msg.isRedelivered();
//        msg.isRejectedBy();
//        msg.isTaken();

        //Header setup

        List<List> data = new LinkedList<List>();

        List<String> id = new LinkedList<String>();
        data.add(id);
        id.add(Columns.ID.name());
        id.add(Console.ROW_DIVIDER);

        List<String> exchange = new LinkedList<String>();
        List<String> routingkey = new LinkedList<String>();
        List<String> immediate = new LinkedList<String>();
        List<String> mandatory = new LinkedList<String>();
        if (showRouting)
        {
            data.add(exchange);
            exchange.add(Columns.Exchange.name());
            exchange.add(Console.ROW_DIVIDER);

            data.add(routingkey);
            routingkey.add(Columns.RoutingKey.name());
            routingkey.add(Console.ROW_DIVIDER);

            data.add(immediate);
            immediate.add(Columns.isImmediate.name());
            immediate.add(Console.ROW_DIVIDER);

            data.add(mandatory);
            mandatory.add(Columns.isMandatory.name());
            mandatory.add(Console.ROW_DIVIDER);
        }

        List<String> size = new LinkedList<String>();
        List<String> appid = new LinkedList<String>();
        List<String> clusterid = new LinkedList<String>();
        List<String> contenttype = new LinkedList<String>();
        List<String> correlationid = new LinkedList<String>();
        List<String> deliverymode = new LinkedList<String>();
        List<String> encoding = new LinkedList<String>();
        List<String> arrival = new LinkedList<String>();
        List<String> expiration = new LinkedList<String>();
        List<String> priority = new LinkedList<String>();
        List<String> propertyflag = new LinkedList<String>();
        List<String> replyto = new LinkedList<String>();
        List<String> timestamp = new LinkedList<String>();
        List<String> type = new LinkedList<String>();
        List<String> userid = new LinkedList<String>();
        List<String> ispersitent = new LinkedList<String>();
        List<String> isredelivered = new LinkedList<String>();
        List<String> isdelivered = new LinkedList<String>();

        data.add(size);
        size.add(Columns.Size.name());
        size.add(Console.ROW_DIVIDER);

        if (showHeaders)
        {
            data.add(ispersitent);
            ispersitent.add(Columns.isPersistent.name());
            ispersitent.add(Console.ROW_DIVIDER);

            data.add(isredelivered);
            isredelivered.add(Columns.isRedelivered.name());
            isredelivered.add(Console.ROW_DIVIDER);

            data.add(isdelivered);
            isdelivered.add(Columns.isDelivered.name());
            isdelivered.add(Console.ROW_DIVIDER);

            data.add(appid);
            appid.add(Columns.App_ID.name());
            appid.add(Console.ROW_DIVIDER);

            data.add(clusterid);
            clusterid.add(Columns.Cluster_ID.name());
            clusterid.add(Console.ROW_DIVIDER);

            data.add(contenttype);
            contenttype.add(Columns.Content_Type.name());
            contenttype.add(Console.ROW_DIVIDER);

            data.add(correlationid);
            correlationid.add(Columns.Correlation_ID.name());
            correlationid.add(Console.ROW_DIVIDER);

            data.add(deliverymode);
            deliverymode.add(Columns.Delivery_Mode.name());
            deliverymode.add(Console.ROW_DIVIDER);

            data.add(encoding);
            encoding.add(Columns.Encoding.name());
            encoding.add(Console.ROW_DIVIDER);

            data.add(arrival);
            expiration.add(Columns.Arrival.name());
            expiration.add(Console.ROW_DIVIDER);

            data.add(expiration);
            expiration.add(Columns.Expiration.name());
            expiration.add(Console.ROW_DIVIDER);

            data.add(priority);
            priority.add(Columns.Priority.name());
            priority.add(Console.ROW_DIVIDER);

            data.add(propertyflag);
            propertyflag.add(Columns.Property_Flag.name());
            propertyflag.add(Console.ROW_DIVIDER);

            data.add(replyto);
            replyto.add(Columns.ReplyTo.name());
            replyto.add(Console.ROW_DIVIDER);

            data.add(timestamp);
            timestamp.add(Columns.Timestamp.name());
            timestamp.add(Console.ROW_DIVIDER);

            data.add(type);
            type.add(Columns.Type.name());
            type.add(Console.ROW_DIVIDER);

            data.add(userid);
            userid.add(Columns.UserID.name());
            userid.add(Console.ROW_DIVIDER);
        }

        List<String> msgHeaders = new LinkedList<String>();
        if (showMessageHeaders)
        {
            data.add(msgHeaders);
            msgHeaders.add(Columns.MsgHeaders.name());
            msgHeaders.add(Console.ROW_DIVIDER);
        }

        //Add create the table of data
        for (QueueEntry entry : messages)
        {
            ServerMessage msg = entry.getMessage();
            if (!includeMsg(msg, msgids))
            {
                continue;
            }

            id.add(msg.getMessageNumber().toString());

            size.add("" + msg.getSize());

            arrival.add("" + msg.getArrivalTime());

            ispersitent.add(msg.isPersistent() ? "true" : "false");


            isredelivered.add(entry.isRedelivered() ? "true" : "false");

            isdelivered.add(entry.getDeliveredToConsumer() ? "true" : "false");

//        msg.getMessageHandle();

            BasicContentHeaderProperties headers = null;

            try
            {
                if(msg instanceof AMQMessage)
                {
                    headers = ((BasicContentHeaderProperties) ((AMQMessage)msg).getContentHeaderBody().properties);
                }
            }
            catch (AMQException e)
            {
                //ignore
//                commandError("Unable to read properties for message: " + e.getMessage(), null);
            }

            if (headers != null)
            {
                String appidS = headers.getAppIdAsString();
                appid.add(appidS == null ? "null" : appidS);

                String clusterS = headers.getClusterIdAsString();
                clusterid.add(clusterS == null ? "null" : clusterS);

                String contentS = headers.getContentTypeAsString();
                contenttype.add(contentS == null ? "null" : contentS);

                String correlationS = headers.getCorrelationIdAsString();
                correlationid.add(correlationS == null ? "null" : correlationS);

                deliverymode.add("" + headers.getDeliveryMode());

                AMQShortString encodeSS = headers.getEncoding();
                encoding.add(encodeSS == null ? "null" : encodeSS.toString());

                expiration.add("" + headers.getExpiration());

                FieldTable headerFT = headers.getHeaders();
                msgHeaders.add(headerFT == null ? "none" : "" + headerFT.toString());

                priority.add("" + headers.getPriority());
                propertyflag.add("" + headers.getPropertyFlags());

                AMQShortString replytoSS = headers.getReplyTo();
                replyto.add(replytoSS == null ? "null" : replytoSS.toString());

                timestamp.add("" + headers.getTimestamp());

                AMQShortString typeSS = headers.getType();
                type.add(typeSS == null ? "null" : typeSS.toString());

                AMQShortString useridSS = headers.getUserId();
                userid.add(useridSS == null ? "null" : useridSS.toString());

                MessagePublishInfo info = null;
                try
                {
                    if(msg instanceof AMQMessage)
                    {
                        info = ((AMQMessage)msg).getMessagePublishInfo();
                    }

                }
                catch (AMQException e)
                {
                    //ignore
                }

                if (info != null)
                {
                    AMQShortString exchangeSS = info.getExchange();
                    exchange.add(exchangeSS == null ? "null" : exchangeSS.toString());

                    AMQShortString routingkeySS = info.getRoutingKey();
                    routingkey.add(routingkeySS == null ? "null" : routingkeySS.toString());

                    immediate.add(info.isImmediate() ? "true" : "false");
                    mandatory.add(info.isMandatory() ? "true" : "false");
                }

//        msg.getPublisher(); -- only used in clustering
//        msg.getStoreContext();
//        msg.isAllContentReceived();

            }// if headers!=null

// need to access internal map and do lookups.
//        msg.isTaken();
//        msg.getDeliveredSubscription();
//        msg.isRejectedBy();

        }

        // if id only had the header and the divider in it then we have no data to display
        if (id.size() == 2)
        {
            return null;
        }
        return data;
    }

    protected boolean includeMsg(ServerMessage msg, List<Long> msgids)
    {
        if (msgids == null)
        {
            return true;
        }

        Long msgid = msg.getMessageNumber();

        boolean found = false;

        if (msgids != null)
        {
            //check msgid is in msgids
            for (Long l : msgids)
            {
                if (l.equals(msgid))
                {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    public enum Columns
    {
        ID,
        Size,
        Exchange,
        RoutingKey,
        isImmediate,
        isMandatory,
        isPersistent,
        isRedelivered,
        isDelivered,
        App_ID,
        Cluster_ID,
        Content_Type,
        Correlation_ID,
        Delivery_Mode,
        Encoding,
        Arrival,
        Expiration,
        Priority,
        Property_Flag,
        ReplyTo,
        Timestamp,
        Type,
        UserID,
        MsgHeaders
    }
}


