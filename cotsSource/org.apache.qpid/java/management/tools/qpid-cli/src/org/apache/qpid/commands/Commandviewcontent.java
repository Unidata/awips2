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

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;

import org.apache.qpid.commands.objects.QueueObject;
import org.apache.qpid.utils.JMXinfo;

public class Commandviewcontent extends CommandImpl
{

    public static final String COMMAND_NAME = "viewcontent";

    private long number = 0;
    private QueueObject objname;
    private MBeanServerConnection mbsc;
    private String method1;
    private ObjectName queue;

    public Commandviewcontent(JMXinfo info)
    {
        super(info);
        this.mbsc = info.getmbserverconnector();
        this.objname = new QueueObject(mbsc);
        this.method1 = "viewMessageContent";

    }

    public void viewcontent()
    {
        Set set = null;
        Object temp[] = { null };
        objname.setQueryString(getObject(), getName(), getVirtualhost());
        set = objname.returnObjects();
        String temp_header = "", header = "", message_data = "", encoding = null;

        if (objname.getSet().size() != 0)
        {
            Iterator it = set.iterator();
            this.queue = (ObjectName) it.next();
            try
            {
                if (objname.getmessagecount(this.queue) == 0)
                {
                    echo("Selected Queue doesn't contain any messages");
                    return;
                }
                if (this.number == 0)
                {
                    echo("You haven't selected a MessageId Please use -id and give a message id");
                    echo("Or run view command with same arguemnts to view messageId list for the queue");
                }
                else
                {
                    Object[] params = { this.number };
                    String[] signature = { new String("long") };
                    CompositeData data = (CompositeData) this.mbsc.invoke(queue, this.method1, params, signature);
                    List<String> itemNames = new ArrayList<String>(data.getCompositeType().keySet());
                    for (int j = 0; j < itemNames.size(); j++)
                    {
                        temp_header = "";
                        temp_header = itemNames.get(j);
                        while (temp_header.length() < 15)
                            temp_header = " " + temp_header;

                        header = header + temp_header + "|";
                    }
                    echo(header);
                    encoding = String.valueOf(data.get(itemNames.get(2))); // set
                                                                           // the
                                                                           // encoding
                                                                           // at
                                                                           // the
                                                                           // beginning
                                                                           // because
                                                                           // encoding
                                                                           // comes
                                                                           // later
                                                                           // in
                                                                           // the
                                                                           // loop
                    if (encoding == null || encoding.length() == 0)
                    {
                        encoding = Charset.defaultCharset().name();

                    }
                    for (int j = 0; j < itemNames.size(); j++)
                    {
                        temp_header = "";
                        if (j != 1)
                        {
                            temp_header = String.valueOf(data.get(itemNames.get(j)));
                            while (temp_header.length() < 15)
                                temp_header = " " + temp_header;
                        }
                        else
                        {
                            Byte[] arrayItems = (Byte[]) data.get(itemNames.get(j));
                            byte[] byteArray = new byte[arrayItems.length];
                            for (int i = 0; i < arrayItems.length; i++)
                            {
                                byteArray[i] = arrayItems[i];
                                temp_header = new String(byteArray, encoding);
                                while (temp_header.length() < 15)
                                    temp_header = " " + temp_header;
                            }
                        }
                        message_data = message_data + temp_header + "|";

                    }
                    echo(message_data);
                }
            }
            catch (Exception ex)
            {
                echo("Given MessageId is invalid, There's no message with the given messageId");
                ex.printStackTrace();
                return;
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
            if (checkoptionsetting("messageid") || checkoptionsetting("id"))
            {
                String number = optionchecker("id");
                if (number == null)
                    number = optionchecker("id");

                setnumber(removeSpaces(number));
            }
            this.viewcontent();
        }
        else if (checkoptionsetting("h") || checkoptionsetting("help"))
            printusage();
        else
            unrecognizeoption();
    }

    public void printusage()
    {
        echo("");
        echo("Usage:viewcontent [OPTION] ... [OBJECT TYPE]...\n");
        echo("view the information about given number of messages from the given queue object\n");
        echo("To specify the desired queue you have to give the virtualhost name and the queue name with following commands\n");
        echo("Where possible options include:\n");
        echo("        -v      --virtualhost Give the virtuallhost name of the desired queue");
        echo("        -n      --name        Give the queue name of the desired queue you want to view messages");
        echo("        -id                   Give the messageId of the required message you want to view the content");
        echo("        -h      --help        Display the help and back to the qpid-cli prompt\n");

    }

    private void setnumber(String number)
    {
        this.number = Long.valueOf(number);
    }

    private static String removeSpaces(String s)
    {
        StringTokenizer st = new StringTokenizer(s, " ", false);
        String t = "";
        while (st.hasMoreElements())
            t += st.nextElement();
        return t;
    }

    public long getnumber()
    {
        return this.number;
    }

}
