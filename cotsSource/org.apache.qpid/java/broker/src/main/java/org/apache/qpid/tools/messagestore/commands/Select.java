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

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.tools.messagestore.MessageStoreTool;

import java.util.LinkedList;
import java.util.StringTokenizer;

public class Select extends AbstractCommand
{

    public Select(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Perform a selection.";
    }

    public String usage()
    {
        return "select virtualhost <name> |exchange <name> |queue <name> | msg id=<msgids eg. 1,2,4-10>";
    }

    public String getCommand()
    {
        return "select";
    }

    public void execute(String... args)
    {
        assert args.length > 2;
        assert args[0].equals("select");

        if (args.length < 3)
        {
            if (args[1].equals("show"))
            {
                doSelect(args[1], null);
            }
            else
            {
                _console.print("select : unknown command:");
                _console.println(help());
            }
        }
        else
        {
            if (args[1].equals("virtualhost")
                || args[1].equals("vhost")
                || args[1].equals("exchange")
                || args[1].equals("queue")
                || args[1].equals("msg")
                    )
            {
                doSelect(args[1], args[2]);
            }
            else
            {
                _console.println(help());
            }
        }
    }

    private void doSelect(String type, String item)
    {
        if (type.equals("virtualhost"))
        {

            VirtualHost vhost = ApplicationRegistry.getInstance()
                    .getVirtualHostRegistry().getVirtualHost(item);

            if (vhost == null)
            {
                _console.println("Virtualhost '" + item + "' not found.");
            }
            else
            {
                _tool.getState().setVhost(vhost);
            }
        }

        if (type.equals("exchange"))
        {

            VirtualHost vhost = _tool.getState().getVhost();

            if (vhost == null)
            {
                _console.println("No Virtualhost open. Open a Virtualhost first.");
                return;
            }


            Exchange exchange = vhost.getExchangeRegistry().getExchange(new AMQShortString(item));

            if (exchange == null)
            {
                _console.println("Exchange  '" + item + "' not found.");
            }
            else
            {
                _tool.getState().setExchange(exchange);
            }

            if (_tool.getState().getQueue() != null)
            {
                if (!exchange.isBound(_tool.getState().getQueue()))
                {
                    _tool.getState().setQueue(null);
                }
            }
        }

        if (type.equals("queue"))
        {
            VirtualHost vhost = _tool.getState().getVhost();

            if (vhost == null)
            {
                _console.println("No Virtualhost open. Open a Virtualhost first.");
                return;
            }

            AMQQueue queue = vhost.getQueueRegistry().getQueue(new AMQShortString(item));

            if (queue == null)
            {
                _console.println("Queue '" + item + "' not found.");
            }
            else
            {
                _tool.getState().setQueue(queue);

                if (_tool.getState().getExchange() == null)
                {
                    for (AMQShortString exchangeName : vhost.getExchangeRegistry().getExchangeNames())
                    {
                        Exchange exchange = vhost.getExchangeRegistry().getExchange(exchangeName);
                        if (exchange.isBound(queue))
                        {
                            _tool.getState().setExchange(exchange);
                            break;
                        }
                    }
                }

                //remove the message selection
                _tool.getState().setMessages((java.util.List<Long>) null);
            }
        }

        if (type.equals("msg"))
        {
            if (item.startsWith("id="))
            {
                StringTokenizer tok = new StringTokenizer(item.substring(item.indexOf("=") + 1), ",");

                java.util.List<Long> msgids = null;

                if (tok.hasMoreTokens())
                {
                    msgids = new LinkedList<Long>();
                }

                while (tok.hasMoreTokens())
                {
                    String next = tok.nextToken();
                    if (next.contains("-"))
                    {
                        Long start = Long.parseLong(next.substring(0, next.indexOf("-")));
                        Long end = Long.parseLong(next.substring(next.indexOf("-") + 1));

                        if (end >= start)
                        {
                            for (long l = start; l <= end; l++)
                            {
                                msgids.add(l);
                            }
                        }
                    }
                    else
                    {
                        msgids.add(Long.parseLong(next));
                    }
                }

                _tool.getState().setMessages(msgids);
            }

        }

        if (type.equals("show"))
        {
            _console.println(_tool.getState().toString());
            if (_tool.getState().getMessages() != null)
            {
                _console.print("Msgs:");
                for (Long l : _tool.getState().getMessages())
                {
                    _console.print(" " + l);
                }
                _console.println("");
            }
        }
    }
}
