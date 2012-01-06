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

import java.util.Set;

import javax.management.MBeanServerConnection;

import org.apache.qpid.commands.objects.AllObjects;
import org.apache.qpid.commands.objects.ConnectionObject;
import org.apache.qpid.commands.objects.ExchangeObject;
import org.apache.qpid.commands.objects.ObjectNames;
import org.apache.qpid.commands.objects.QueueObject;
import org.apache.qpid.commands.objects.UserManagementObject;
import org.apache.qpid.commands.objects.VirtualHostObject;
import org.apache.qpid.utils.JMXinfo;

public class Commandlist extends CommandImpl
{

    public static final String COMMAND_NAME = "list";

    public Commandlist(JMXinfo info)
    {
        super(info);
    }

    private void listobjects(String option_value)
    {
        /*
         * pring usage if use is not give the correct option letter or no
         * options
         */
        if (option_value == null)
        {
            // System.out.println("testing");
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
            else if (option_value.compareToIgnoreCase("Virtualhosts") == 0
                    || option_value.compareToIgnoreCase("Virtualhost") == 0)
            {
                objname = new VirtualHostObject(mbsc);
                // this.name = option_value;
            }
            else if (option_value.compareToIgnoreCase("Exchange") == 0
                    || option_value.compareToIgnoreCase("Exchanges") == 0)
            {
                objname = new ExchangeObject(mbsc);
                // this.name = option_value;
            }
            else if (option_value.compareToIgnoreCase("Connection") == 0
                    || option_value.compareToIgnoreCase("Connections") == 0)
            {
                objname = new ConnectionObject(mbsc);
                // this.name = option_value;
            }
            else if (option_value.compareToIgnoreCase("all") == 0)
            {
                objname = new AllObjects(mbsc);
                // this.name = option_value;
            }
            else if (option_value.compareToIgnoreCase("Usermanagement") == 0
                    || option_value.compareToIgnoreCase("Usermanagmenets") == 0)
            {
                objname = new UserManagementObject(mbsc);
                // this.name = option_value;
            }
            else
            {
                printusage();
                echo("Wrong objectName");
                return;
            }
            objname.setQueryString(this.getObject(), this.getName(), this.getVirtualhost());
            objname.returnObjects();
            if (objname.getSet().size() != 0)
            {
                if (this.getObject().compareToIgnoreCase("queue") == 0
                        || this.getObject().compareToIgnoreCase("queues") == 0)
                    objname.displayqueues(this.getOutputFormat(), this.getSeperator());
                else
                    objname.displayobjects(this.getOutputFormat(), this.getSeperator());
            }
            else
            {
                if (hasName())
                {

                    echo("You might quering wrong " + this.getObject() + " name with --name or -n option ");
                    echo("");
                    echo(this.getObject() + "Type Objects might not in the broker currently");
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

    public void listdomains()
    {
        MBeanServerConnection mbsc = info.getmbserverconnector();
        try
        {
            String[] domains = mbsc.getDomains();
            echo("DOMAINS");
            for (int i = 0; i < domains.length; i++)
                echo("\tDomain[" + i + "] = " + domains[i]);

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
                setSeperator(optionchecker("separator"));
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
                    name = optionchecker("n");

                setName(name);
            }
            if (checkoptionsetting("virtualhost") || checkoptionsetting("v"))
            {
                String vhost = optionchecker("virtualhost");
                if (vhost == null)
                    vhost = optionchecker("v");
                setVirtualhost(vhost);
            }
            listobjects(this.getObject());
        }
        else if (checkoptionsetting("domain") || checkoptionsetting("d"))
            listdomains();
        else if (checkoptionsetting("h") || checkoptionsetting("help"))
            printusage();
        else
            unrecognizeoption();
    }

    public void printusage()
    {
        echo("");
        echo("Usage:list [OPTION] ... [OBJECT TYPE]...\n");
        echo("List the information about the given object\n");
        echo("Where possible options include:\n");
        echo("        -o      --object      type of objects which you want to list\n");
        echo("                              ex: < list -o queue > : lists all the queues created in the java broker\n");
        echo("                              For now list command is supporting following object typse \n");
        echo("                              Queue  Connection  VirtualHost  UserMangement  Exchange");
        echo("                              Or You can specify object type by giving it at the beginning");
        echo("                              rather giving it as a argument");
        echo("                              Ex:< queue list > this command is equal to list -o queue \n");
        echo("        -d      --domain      list all the domains of objects available for remote monitoring\n");
        echo("        -v      --virtualhost After specifying the object type you can filter output with this option");
        echo("                              list objects with the given virtualhost which will help to find ");
        echo("                              identical queue objects with -n option");
        echo("                              ex: queue list -v develop   ment");
        echo("        -output               Specify which output format you want to get the ouput");
        echo("                              Although the option is there current version supports only for CSV output format");
        echo("        -separator            This option use with output option to specify which separator you want to get the CSV output (default seperator is comma");
        echo("        -h      --help        Display the help and back to the qpid-cli prompt\n");
        echo("        -n      --name        After specifying what type of objects you want to monitor you can filter");
        echo("                              the output using -n option by specifying the name of the object you want ");
        echo("                              to monitor exactly");
        echo("                              ex: <list -o queue -n ping> : list all the queue objects having queue name");
        echo("                              of ping");
        echo("                              ex: <queue list -n ping -v development> list all the queue objects with name ");
        echo("                              of ping and virtualhost of developement \n");

    }
}
