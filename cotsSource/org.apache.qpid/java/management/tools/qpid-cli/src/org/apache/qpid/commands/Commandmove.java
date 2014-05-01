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

import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;

import org.apache.qpid.commands.objects.QueueObject;
import org.apache.qpid.utils.JMXinfo;

public class Commandmove extends CommandImpl
{

    public static final String COMMAND_NAME = "move";

    private String name1 = null, name2 = null, vhost1 = null, vhost2 = null, method1 = null, method2 = null; // target
                                                                                                             // and
                                                                                                             // starting
                                                                                                             // queue
                                                                                                             // specifications
                                                                                                             // happen
                                                                                                             // with
                                                                                                             // these
                                                                                                             // options
    private QueueObject queue1;
    private MBeanServerConnection mbsc;
    private ObjectName queue;
    private int fmid = 0, tmid = 0;

    public Commandmove(JMXinfo info)
    {
        super(info);

        this.mbsc = info.getmbserverconnector();
        this.queue1 = new QueueObject(mbsc);
        this.method1 = "moveMessages";
        this.method2 = "getMessagesOnTheQueue";

    }

    public void movemessages()
    {
        Set set = null;
        queue1.setQueryString(this.getObject(), this.name1, this.vhost1);
        set = queue1.returnObjects();
        if (queue1.getSet().size() != 0)
        { // find the queue
            Iterator it = set.iterator();
            this.queue = (ObjectName) it.next();
        }
        else
        {
            if (isname1() || isname2())
            { // if the specified queue is not there in the broker

                echo("The Queue you have specified is not in the current broker");
                echo("");
            }
            else
            {
                printusage();
            }
        }
        try
        {
            Object[] params1 = { getfmid(), gettmid(), this.name2 };
            String[] signature1 = { new String("long"), new String("long"), new String("java.lang.String") };
            this.mbsc.invoke(this.queue, this.method1, params1, signature1);

        }
        catch (Exception ex)
        {
            ex.printStackTrace();
            echo("Given messageId's might be wrong please run the view command and check messageId's you have given\n");
            echo("From MessageId should be greater than 0 and should less than To messageId");
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
                echo("This command is only applicable for queue command so please start with queue");
            }
            if (checkoptionsetting("n2") && checkoptionsetting("n1"))
            {
                setname1(optionchecker("n1"));
                setname2(optionchecker("n2"));
            }
            else
            {
                echo("You have to specify both n1 and n2 option value to move a message"); /*
                                                                                            * when
                                                                                            * user
                                                                                            * forget
                                                                                            * to
                                                                                            * specify
                                                                                            * target
                                                                                            * or
                                                                                            * starting
                                                                                            * queue
                                                                                            * name
                                                                                            */
                return;
            }

            if (checkoptionsetting("v1"))
            {

                setvhost1(optionchecker("v1"));
            }
            if (checkoptionsetting("tmid") && checkoptionsetting("fmid"))
            {
                String tmid = optionchecker("tmid");
                String fmid = optionchecker("fmid");

                settomessageIdandfrommessageId(removeSpaces(tmid), removeSpaces(fmid));
            }
            else
            {
                echo("You have to set from MessageId and to MessageId in order to move messages between queues");
                echo("To view MessageId's use <view> command with -n and -v options");
                return;
            }
            this.movemessages();

        }
        else if (checkoptionsetting("h") || checkoptionsetting("help"))
            printusage();
        else
            unrecognizeoption();
    }

    public void printusage()
    {
        echo("");
        echo("Usage:move [OPTION] ... [OBJECT TYPE]...\n");
        echo("Move the top most messages from the given queue object to the given destination object\n");
        echo("To specify the desired queues you have to give the virtualhost name and the queue name with following commands\n");
        echo("Where possible options include:\n");
        echo("        -v1                   Give the virtuallhost name from which queue you want to move messages");
        echo("        -n1                   Give the queue name which you want to move messages from");
        echo("        -n2                   Give the queue name of the destination queue");
        echo("        -tmid                 Give From MessageId you want to move from the Queue");
        echo("        -fmid                 Give To MessageId you want to move from the Queue");
        echo("        -h      --help        Display the help and back to the qpid-cli prompt\n");

    }

    private void setname1(String name)
    {
        this.name1 = name;
    }

    private void setname2(String name)
    {
        this.name2 = name;
    }

    private boolean isname1()
    {
        if (this.name1 == null)
            return false;

        else
            return true;
    }

    private boolean isname2()
    {
        if (this.name2 == null)
            return false;

        else
            return true;
    }

    private void setvhost1(String vhost)
    {
        this.vhost1 = vhost;
    }

    private static String removeSpaces(String s)
    {
        StringTokenizer st = new StringTokenizer(s, " ", false);
        String t = "";
        while (st.hasMoreElements())
            t += st.nextElement();
        return t;
    }

    private void settomessageIdandfrommessageId(String tmid, String fmid)
    {
        Integer i = new Integer(tmid);
        Integer j = new Integer(fmid);
        this.tmid = i.intValue();
        this.fmid = j.intValue();
    }

    public int gettmid()
    {
        return this.tmid;
    }

    public int getfmid()
    {
        return this.fmid;
    }

    public String getname1()
    {
        return this.name1;
    }

    public String getname2()
    {
        return this.name2;
    }

}
