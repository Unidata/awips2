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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;

import org.apache.qpid.commands.objects.QueueObject;
import org.apache.qpid.utils.JMXinfo;

public class Commanddelete extends CommandImpl
{
    private int number = 0;
    private QueueObject objname;
    private MBeanServerConnection mbsc;
    private String method1, method2;
    private ObjectName queue;
    public static final String COMMAND_NAME = "delete";

    public Commanddelete(JMXinfo info)
    {
        super(info);
        this.mbsc = info.getmbserverconnector();
        this.objname = new QueueObject(mbsc);
        this.method1 = "deleteMessageFromTop";
        this.method2 = "clearQueue";

    }

    public void deletemessages()
    {
        Set set = null;
        objname.setQueryString(this.getObject(), this.getName(), this.getVirtualhost());
        set = objname.returnObjects();

        if (objname.getSet().size() != 0)
        {
            Iterator it = set.iterator();
            this.queue = (ObjectName) it.next();
            try
            {
                if (this.number == 0)
                {
                    echo("");
                    System.out.print("Do you want to delete all the messages from the Queue [Y/N] :");
                    InputStreamReader isr = new InputStreamReader(System.in);
                    BufferedReader br = new BufferedReader(isr);
                    String s = br.readLine();
                    echo(s);
                    if (s.compareToIgnoreCase("y") == 0)
                        this.mbsc.invoke(queue, this.method2, null, null);
                    else
                        return;
                }
                else if (objname.getmessagecount(this.queue) < this.number)
                {
                    echo("Given number is Greater than the Queue Depth");
                    return;
                }
                else
                {
                    for (int i = 0; i < this.number; i++)
                    {
                        this.mbsc.invoke(queue, this.method1, null, null);
                    }
                }
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }

        }
        else
        {
            if (hasName())
            {

                echo("The Queue you have specified is not in the current broker");
                echo("");
            }
            else
            {
                printusage();
            }
        }
    }

    public void execute()
    {
        /*
         * In here you it's easy to handle any number of otpions which are going
         * to add with the list command which works with main option object or o
         */

        if (checkoptionsetting("object") || checkoptionsetting("o"))
        {
            String object = optionchecker("object");
            if (object == null)
            {
                object = optionchecker("o");
            }
            if (object.compareToIgnoreCase("queue") == 0)
                setObject(object);
            else
            {
                unrecognizeoption();
                echo("This command is only applicable for delete command so please start with queue");
            }
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
            if (checkoptionsetting("top") || checkoptionsetting("t"))
            {
                String number = optionchecker("top");
                if (number == null)
                    number = optionchecker("t");

                setnumber(removeSpaces(number));
            }
            this.deletemessages();
        }
        else if (checkoptionsetting("h") || checkoptionsetting("help"))
            printusage();
        else
            unrecognizeoption();
    }

    public void printusage()
    {
        echo("");
        echo("Usage:delete [OPTION] ... [OBJECT TYPE]...\n");
        echo("Delete the top most messages from the given queue object\n");
        echo("To specify the desired queue you have to give the virtualhost name and the queue name with following commands\n");
        echo("Where possible options include:\n");
        echo("        -v      --virtualhost Give the virtuallhost name of the desired queue");
        echo("        -n      --name        Give the queue name of the desired queue you want to do the delete operation");
        echo("        -t      --top         Give how many number of messages you want to delete from the top (Default = all the messages will be deleted");
        echo("        -h      --help        Display the help and back to the qpid-cli prompt\n");

    }

    private void setnumber(String number)
    {
        Integer i = new Integer(number);
        this.number = i.intValue();
    }

    public int getnumber()
    {
        return this.number;
    }

    private static String removeSpaces(String s)
    {
        StringTokenizer st = new StringTokenizer(s, " ", false);
        String t = "";
        while (st.hasMoreElements())
            t += st.nextElement();
        return t;
    }
}
