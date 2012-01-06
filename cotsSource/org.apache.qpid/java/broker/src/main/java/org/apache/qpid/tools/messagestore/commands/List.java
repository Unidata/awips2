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
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.tools.messagestore.MessageStoreTool;
import org.apache.qpid.tools.utils.Console;

import java.util.Collection;
import java.util.LinkedList;

public class List extends AbstractCommand
{

    public List(MessageStoreTool tool)
    {
        super(tool);
    }

    public void setOutput(Console out)
    {
        _console = out;
    }

    public String help()
    {
        return "list available items.";
    }

    public String usage()
    {
        return "list queues [<exchange>] | exchanges | bindings [<exchange>] | all";
    }

    public String getCommand()
    {
        return "list";
    }

    public void execute(String... args)
    {
        assert args.length > 0;
        assert args[0].equals(getCommand());

        if (args.length > 1)
        {
            if ((args[1].equals("exchanges"))
                || (args[1].equals("queues"))
                || (args[1].equals("bindings"))
                || (args[1].equals("all")))
            {
                if (args.length == 2)
                {
                    doList(args[1]);
                }
                else if (args.length == 3)
                {
                    doList(args[1], args[2]);
                }
            }
            else
            {
                commandError("Unknown options. ", args);
            }
        }
        else if (args.length < 2)
        {
            doList("all");
        }
        else
        {
            doList(args[1]);
        }
    }

    private void doList(String... listItem)
    {
        if (_tool.getState().getVhost() == null)
        {
            _console.println("No Virtualhost open. Open a Virtualhost first.");
            listVirtualHosts();
            return;
        }

        VirtualHost vhost = _tool.getState().getVhost();

        java.util.List<String> data = null;

        if (listItem[0].equals("queues"))
        {
            if (listItem.length > 1)
            {
                data = listQueues(vhost, new AMQShortString(listItem[1]));
            }
            else
            {
                Exchange exchange = _tool.getState().getExchange();
                data = listQueues(vhost, exchange);
            }
        }

        if (listItem[0].equals("exchanges"))
        {
            data = listExchanges(vhost);
        }

        if (listItem[0].equals("bindings"))
        {

            if (listItem.length > 1)
            {
                data = listBindings(vhost, new AMQShortString(listItem[1]));
            }
            else
            {
                Exchange exchange = _tool.getState().getExchange();

                data = listBindings(vhost, exchange);
            }
        }

        if (data != null)
        {
            if (data.size() == 1)
            {
                _console.println("No '" + listItem[0] + "' to display,");
            }
            else
            {
                _console.displayList(true, data.toArray(new String[0]));
            }
        }


        if (listItem[0].equals("all"))
        {

            boolean displayed = false;
            Exchange exchange = _tool.getState().getExchange();

            //Do the display here for each one so that they are pretty printed
            data = listQueues(vhost, exchange);
            if (data != null)
            {
                displayed = true;
                _console.displayList(true, data.toArray(new String[0]));
            }

            if (exchange == null)
            {
                data = listExchanges(vhost);
                if (data != null)
                {
                    displayed = true;
                    _console.displayList(true, data.toArray(new String[0]));
                }
            }

            data = listBindings(vhost, exchange);
            if (data != null)
            {
                displayed = true;
                _console.displayList(true, data.toArray(new String[0]));
            }

            if (!displayed)
            {
                _console.println("Nothing to list");
            }
        }
    }

    private void listVirtualHosts()
    {
        Collection<VirtualHost> vhosts = ApplicationRegistry.getInstance()
                .getVirtualHostRegistry().getVirtualHosts();

        String[] data = new String[vhosts.size() + 1];

        data[0] = "Available VirtualHosts";

        int index = 1;
        for (VirtualHost vhost : vhosts)
        {
            data[index] = vhost.getName();
            index++;
        }

        _console.displayList(true, data);
    }

    private java.util.List<String> listBindings(VirtualHost vhost, AMQShortString exchangeName)
    {
        return listBindings(vhost, vhost.getExchangeRegistry().getExchange(exchangeName));
    }

    private java.util.List<String> listBindings(VirtualHost vhost, Exchange exchange)
    {
        Collection<AMQShortString> queues = vhost.getQueueRegistry().getQueueNames();

        if (queues == null || queues.size() == 0)
        {
            return null;
        }

        java.util.List<String> data = new LinkedList<String>();

        data.add("Current Bindings");

        for (AMQShortString queue : queues)
        {
            if (exchange != null)
            {
                if (exchange.isBound(queue))
                {
                    data.add(queue.toString());
                }
            }
            else
            {
                data.add(queue.toString());
            }
        }

        return data;
    }

    private java.util.List<String> listExchanges(VirtualHost vhost)
    {
        Collection<AMQShortString> queues = vhost.getExchangeRegistry().getExchangeNames();

        if (queues == null || queues.size() == 0)
        {
            return null;
        }

        java.util.List<String> data = new LinkedList<String>();

        data.add("Available Exchanges");

        for (AMQShortString queue : queues)
        {
            data.add(queue.toString());
        }

        return data;
    }

    private java.util.List<String> listQueues(VirtualHost vhost, AMQShortString exchangeName)
    {
        return listQueues(vhost, vhost.getExchangeRegistry().getExchange(exchangeName));
    }

    private java.util.List<String> listQueues(VirtualHost vhost, Exchange exchange)
    {
        Collection<AMQQueue> queues = vhost.getQueueRegistry().getQueues();

        if (queues == null || queues.size() == 0)
        {
            return null;
        }

        java.util.List<String> data = new LinkedList<String>();

        data.add("Available Queues");

        for (AMQQueue queue : queues)
        {
            if (exchange != null)
            {
                if (exchange.isBound(queue))
                {
                    data.add(queue.getName().toString());
                }
            }
            else
            {
                data.add(queue.getName().toString());
            }
        }

        if (exchange != null)
        {
            if (queues.size() == 1)
            {
                return null;
            }
        }

        return data;
    }
}
