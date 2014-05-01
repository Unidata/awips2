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

package org.apache.qpid.commands;

import org.apache.qpid.commands.objects.ObjectNames;
import org.apache.qpid.commands.objects.QueueObject;
import org.apache.qpid.utils.JMXinfo;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import java.util.Set;

public class Commandget extends CommandImpl
{

    private String _attributeName;
    private String _value;
    public static String COMMAND_NAME = "get";

    public Commandget(JMXinfo info)
    {
        super(info);
    }

    private void getAttribute(String option_value)
    {
        if (option_value == null)
        {
            printusage();
            return;
        }
        MBeanServerConnection mbsc = info.getmbserverconnector();
        Set set = null;
        ObjectNames objname = null;

        try
        {
            if (option_value.compareToIgnoreCase("queue") == 0 || option_value.compareToIgnoreCase("queues") == 0)
            {
                objname = new QueueObject(mbsc);
            }
            else
            {
                printusage();
                echo("Wrong objectName");
                return;
            }

            if (_attributeName == null)
            {
                echo("attribute name not specified. See --help for details");
                return;
            }

            objname.setQueryString(this.getObject(), this.getName(), this.getVirtualhost());
            objname.returnObjects();

            if (objname.getSet().size() != 1)
            {
                echo("Your query returned more than one object to set was this intended?\n" + objname.getQueryString());
            }
            else if (objname.getSet().size() == 1)
            {
                ObjectName object = (ObjectName) objname.getSet().iterator().next();

                Object value = objname.getAttribute(object, _attributeName);

                echo(value.toString());
            }
            else
            {
                if (hasName())
                {

                    echo("You might be querying wrong " + this.getObject() + " name with --name or -n option ");
                    echo("");
                    echo("No " + this.getObject() + "Type Objects might not in the broker currently");
                    echo("");
                }
                else
                {
                    printusage();
                }
            }

        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }

    }

    public void execute()
    {
        /*
         * In here you it's easy to handle any number of otpions which are going
         * to add with the list command which works with main option object or o
         */
        if (checkoptionsetting("output"))
        {
            setOutputFormat(optionchecker("output"));
            if (checkoptionsetting("separator"))
            {
                setSeperator(optionchecker("separator"));
            }
        }
        if (checkoptionsetting("object") || checkoptionsetting("o"))
        {
            String object = optionchecker("object");
            if (object == null)
            {
                object = optionchecker("o");
            }
            setObject(object);

            if (checkoptionsetting("name") || checkoptionsetting("n"))
            {
                String name = optionchecker("name");
                if (name == null)
                {
                    name = optionchecker("n");
                }

                setName(name);
            }

            if (checkoptionsetting("attribute") || checkoptionsetting("a"))
            {
                String name = optionchecker("attribute");
                if (name == null)
                {
                    name = optionchecker("a");
                }

                setAttributeName(name);
            }
            if (checkoptionsetting("virtualhost") || checkoptionsetting("v"))
            {
                String vhost = optionchecker("virtualhost");
                if (vhost == null)
                {
                    vhost = optionchecker("v");
                }
                setVirtualhost(vhost);
            }
            getAttribute(this.getObject());
        }
        else if (checkoptionsetting("h") || checkoptionsetting("help"))
        {
            printusage();
        }
        else
        {
            unrecognizeoption();
        }
    }

    private void setAttributeName(String name)
    {
        this._attributeName = name;
    }

    public void printusage()
    {
        echo("");
        echo("Usage:set [OPTION] ... [OBJECT TYPE]...\n");
        echo("List the information about the given object\n");
        echo("Where possible options include:\n");
        echo("        -o      --object      type of objects which you want to list\n");
        echo("                              ex: < list -o queue > : lists all the queues created in the java broker\n");
        echo("                              For now list command is supporting following object typse \n");
        echo("                              Queue  Connection  VirtualHost  UserMangement  Exchange");
        echo("                              Or You can specify object type by giving it at the beginning");
        echo("                              rather giving it as a argument");
        echo("                              Ex:< queue list > this command is equal to list -o queue \n");
        echo("        -v      --virtualhost After specifying the object type you can filter output with this option");
        echo("                              list objects with the given virtualhost which will help to find ");
        echo("                              identical queue objects with -n option");
        echo("                              ex: queue list -v develop   ment");
        echo("        -n      --name        After specifying what type of objects you want to monitor you can filter");
        echo("                              the output using -n option by specifying the name of the object you want ");
        echo("                              to monitor exactly");
        echo("                              ex: <list -o queue -n ping> : list all the queue objects having queue name");
        echo("                              of ping");
        echo("                              ex: <queue list -n ping -v development> list all the queue objects with name ");
        echo("                              of ping and virtualhost of developement \n");
        echo("        -a      --attribute   ");
        echo("        -h      --help        Display the help and back to the qpid-cli prompt\n");

    }
}
