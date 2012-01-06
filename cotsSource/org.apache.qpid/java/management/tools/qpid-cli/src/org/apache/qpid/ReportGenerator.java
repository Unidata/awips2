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
package org.apache.qpid;

import org.apache.qpid.commands.objects.*;
import org.apache.qpid.utils.JMXinfo;

import javax.management.MBeanServerConnection;
import java.util.*;
import java.io.FileInputStream;
import java.io.IOException;

public class ReportGenerator implements Runnable
{

    String[] propertyname = { "object", "column", "filter.name", "filter.virtualhost", "output", "seperator",
            "interval" };
    String[] propertyvalue = null;
    String propertyfilepath = null;
    String[] columnvalue = null;
    List<String> columns = null;
    Properties props = null;
    JMXinfo info = null;
    int interval = 10;

    public void run()
    {
        for (;;) // creating infinite loop
        {
            generatereport();
            try
            {
                Thread.sleep(this.interval * 60000);
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }
        }
    }

    public ReportGenerator(String propfile, JMXinfo info)
    {
        this.propertyfilepath = propfile;
        props = new Properties();
        propertyvalue = new String[propertyname.length];
        this.info = info;
        columns = new ArrayList<String>();
    }

    public void loadproperties() // get all the property file values and set the
                                 // columen values//
    {
        try
        {

            props.load(new FileInputStream(this.propertyfilepath));
            for (int i = 0; i < propertyname.length; i++)
            {
                propertyvalue[i] = props.getProperty(propertyname[i]);
            }
            this.setcolumnvalues();
            this.setinterval();
        }

        // catch exception in case properties file does not exist

        catch (IOException e)
        {
            System.out.println("Oooops Give property file is not exist");
        }
    }

    public void generatereport()
    {
        this.listobjects(propertyvalue[0]);
    }

    private void setcolumnvalues()
    {
        columnvalue = propertyvalue[1].split(",");
        for (int i = 0; i < columnvalue.length; i++)
        {
            columns.add(columnvalue[i]);
        }
    }

    private void setinterval()
    {
        this.interval = (new Integer(propertyvalue[6])).intValue();
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
            objname.setQueryString(this.propertyvalue[0], this.propertyvalue[2], this.propertyvalue[3]);
            objname.returnObjects();
            if (objname.getSet().size() != 0)
            {
                objname.reportgenerator(this.propertyvalue[4], this.propertyvalue[5], columns);
            }
            else
            {
                printusage();
            }
        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }

    }

    public void echo(String str)
    {
        System.out.println(str);
    }

    public void printusage()
    {
        System.out.println("Wrong option or wrong option value");
    }
}
