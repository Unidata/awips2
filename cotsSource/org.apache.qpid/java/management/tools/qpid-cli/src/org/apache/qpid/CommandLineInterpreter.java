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

import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;

import jline.ArgumentCompletor;
import jline.ConsoleReader;
import jline.SimpleCompletor;

import org.apache.qpid.commands.Commanddelete;
import org.apache.qpid.commands.Commandget;
import org.apache.qpid.commands.Commandhelp;
import org.apache.qpid.commands.Commandinfo;
import org.apache.qpid.commands.Commandlist;
import org.apache.qpid.commands.Commandmove;
import org.apache.qpid.commands.Commandset;
import org.apache.qpid.commands.Commandview;
import org.apache.qpid.commands.Commandviewcontent;
import org.apache.qpid.utils.CommandLineOptionParser;
import org.apache.qpid.utils.JMXConfiguration;
import org.apache.qpid.utils.JMXinfo;

public class CommandLineInterpreter
{
    private static final String OBJECT_VIRTUALHOST = "virtualhost";
    private static final String OBJECT_USERMANAGEMENT = "usermanagement";
    private static final String OBJECT_CONNECTION = "connection";
    private static final String OBJECT_EXCHANGE = "exchange";
    private static final String OBJECT_QUEUE = "queue";
    private static final String COMMAND_QUIT = "quit";
    private static final String COMMAND_EXIT = "exit";

    public static void main(String[] args)
    {
        Connector conn = null;
        try
        {
            // Create an RMI connector client and
            // connect it to the RMI connector server

            /*
             * checking the commandline options and parse them in to config
             * method
             */
            JMXConnector jmxc = null;
            MBeanServerConnection mbsc = null;
            ConsoleReader reader = new ConsoleReader();
            reader.setBellEnabled(false);
            CommandLineOptionParser commandlineoptionparser = null;

            if ((args == null) || (args.length) == 0)
            {
                Usage();
            }
            /*
             * here special constructor is calling, when parsing options,in here
             * first option value is starting from minus sign so this is handle
             * by a special constructor
             */
            else
            {
                if (args[0].startsWith("-"))
                {
                    commandlineoptionparser = new CommandLineOptionParser(args, args[0]); // if
                                                                                          // user
                                                                                          // specify
                                                                                          // any
                                                                                          // argument
                                                                                          // with
                                                                                          // the
                                                                                          // qpid-cli
                                                                                          // script
                }
            }

            registerCommands();

            /* Connecting with the broker */
            try
            {
                if (commandlineoptionparser == null)
                    commandlineoptionparser = new CommandLineOptionParser(args);

                JMXConfiguration config = new JMXConfiguration(commandlineoptionparser.getAlloptions());
                conn = ConnectorFactory.getConnector(config.gethostname(), config.getport(), config.getUsername(),
                        config.getPassword());
                jmxc = conn.getConnector();
                mbsc = conn.getMBeanServerConnection();
                if (config.checkoptionsetting("r", commandlineoptionparser.getAlloptions()))
                {
                    JMXinfo info = new JMXinfo(jmxc, commandlineoptionparser, mbsc);
                    ReportGenerator reportgen = new ReportGenerator(config.optionchecker("r", commandlineoptionparser
                            .getAlloptions()), info);
                    reportgen.loadproperties();
                    reportgen.run();
                }
                /*
                 * This implementation is for the people who are using the
                 * interactive mode for one shot this run the user given command
                 * and exit
                 */
                for (int i = 0; i < args.length; i++)
                {
                    if (CommandExecutionEngine.getCommands().keySet().contains(args[i]))
                    {
                        oneshotmode(args, commandlineoptionparser, jmxc, mbsc);
                        return;
                    }
                }
            }
            catch (Exception ex)
            {
                connectionrefuse(ex);
                return;
            }
            /* In this point connection has been established */
            String line;
            String[] command;

            /* prividing GNU readline features using Jline library */
            PrintWriter out = new PrintWriter(System.out);
            SimpleCompletor completer = new SimpleCompletor(new String[] { 
                    COMMAND_EXIT, COMMAND_QUIT, OBJECT_QUEUE, OBJECT_EXCHANGE, OBJECT_CONNECTION,
                    OBJECT_USERMANAGEMENT, OBJECT_VIRTUALHOST});
            for (String commandName : CommandExecutionEngine.getCommands().keySet())
            {
                completer.addCandidateString(commandName);
            }
            reader.addCompletor(new ArgumentCompletor(completer));
            while ((line = reader.readLine("qpid-admin-$ ")) != null)
            {
                out.flush();
                if (removeSpaces(line).equalsIgnoreCase(COMMAND_QUIT) || removeSpaces(line).equalsIgnoreCase(COMMAND_EXIT))
                    break;
                else if (line.length() == 0)
                    continue;
                else
                {
                    command = line.split("\\s+");
                    commandlineoptionparser = new CommandLineOptionParser(command);
                    JMXinfo info = new JMXinfo(jmxc, commandlineoptionparser, mbsc);
                    CommandExecutionEngine engine = new CommandExecutionEngine(info);
                    if (engine.CommandSelector())
                        engine.runcommand();
                }
            }

            conn.getConnector().close();
        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }
    }

    private static void registerCommands()
    {
        CommandExecutionEngine.addCommand(Commanddelete.COMMAND_NAME, Commanddelete.class);
        CommandExecutionEngine.addCommand(Commandget.COMMAND_NAME, Commandget.class);
        CommandExecutionEngine.addCommand(Commandhelp.COMMAND_NAME, Commandhelp.class);
        CommandExecutionEngine.addCommand(Commandinfo.COMMAND_NAME, Commandinfo.class);
        CommandExecutionEngine.addCommand(Commandlist.COMMAND_NAME, Commandlist.class);
        CommandExecutionEngine.addCommand(Commandmove.COMMAND_NAME, Commandmove.class);
        CommandExecutionEngine.addCommand(Commandset.COMMAND_NAME, Commandset.class);
        CommandExecutionEngine.addCommand(Commandview.COMMAND_NAME, Commandview.class);
        CommandExecutionEngine.addCommand(Commandviewcontent.COMMAND_NAME, Commandviewcontent.class);
    }

    private static void Usage()
    {
        System.out.println("Connecting to localhost Qpid java broker...");
    }

    private static String removeSpaces(String s)
    {
        StringTokenizer st = new StringTokenizer(s, " ", false);
        String t = "";
        while (st.hasMoreElements())
            t += st.nextElement();
        return t;
    }

    private static void connectionrefuse(Exception e)
    {
        String message = e.getLocalizedMessage();
        if (e instanceof SecurityException)
        {
            message = " authentication failed, please check username and password";
        }
        else
        {
            Throwable cause = e.getCause();
            while (cause != null)
            {
                message = cause.getMessage();
                cause = cause.getCause();
            }
        }

        System.out.println("Cannot connect with the broker: " + message);
    }

    public static String[] oneshotmode(String[] args, CommandLineOptionParser commandlineoptionparser,
            JMXConnector jmxc, MBeanServerConnection mbsc) throws Exception
    {
        int check = 0;
        String[] temp;
        for (int i = 0; i < args.length; i++)
        {

            if (args[i].compareTo("-h") == 0)
                check++;
            else if (args[i].compareTo("-p") == 0)
                check++;
        }
        for (int i = 0; i < (args.length - 2 * check); i++)
        { // mulitply by 2 because have to remove the option letter with the
          // option value//
            args[i] = args[i + check * 2];
        }

        commandlineoptionparser = new CommandLineOptionParser(args); // change
                                                                     // the args
                                                                     // string
                                                                     // array
                                                                     // which
                                                                     // works as
                                                                     // interactive
                                                                     // mode//
        JMXinfo info = new JMXinfo(jmxc, commandlineoptionparser, mbsc);
        CommandExecutionEngine engine = new CommandExecutionEngine(info);
        if (engine.CommandSelector())
            engine.runcommand();
        return args;

    }
}
