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

package org.apache.qpid.utils;

import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

public class CommandLineOptionParser
{
    private static int STARTED = 0;
    private static int NEW_OPTION = 1;
    private static int SUB_PARAM_OF_OPTION = 2;

    private Map commandlineoption;
    private String commandname;

    public CommandLineOptionParser(Map commandlineoptions)
    {
        this.commandlineoption = commandlineoptions;
    }

    public CommandLineOptionParser(String[] args)
    {
        /* check whether user just type the enter key */
        this.commandlineoption = this.parse(args);

    }

    public CommandLineOptionParser(String[] args, String first)
    {
        this.commandname = first;
        this.commandlineoption = this.parsefirst(args);
    }

    public Map parse(String[] args)
    {
        Map commandLineOptions = new HashMap();

        if (0 == args.length)
        {
            return commandLineOptions;
        }
        else if (1 == args.length)
        {
            commandname = args[0];
            return commandLineOptions;
        }
        /* when user is not giving the command line option with a "=" */
        // if (!args[2].startsWith("-"))
        // return commandLineOptions;
        // State 0 means started
        // State 1 means earlier one was a new -option
        // State 2 means earlier one was a sub param of a -option
        int state = STARTED;
        ArrayList optionBundle = null;
        String optionType = null;
        CommandLineOption commandLineOption;
        String newcommand = "";
        String[] newargs;
        int j;
        if (args[1].compareTo("list") == 0 || args[1].compareTo("info") == 0 || args[1].compareTo("delete") == 0
                || args[1].compareTo("move") == 0 || args[1].compareTo("view") == 0
                || args[1].compareTo("viewcontent") == 0)
        {
            String object = args[0];
            for (j = 0; j < (args.length - 1); j++)
            {
                newcommand = newcommand + args[j + 1] + " ";
            }
            newcommand = newcommand + "-o " + object;
            newargs = newcommand.split(" ");
            args = newargs;
        }
        else if (!args[1].startsWith("-")) // if user give command like list
                                           // queue or something without minus
                                           // argument
            return commandLineOptions; // for the second wordxi

        commandname = args[0];
        for (int i = 0; i < args.length; i++)
        {
            if (args[i].startsWith("-"))
            {
                if (STARTED == state)
                {
                    // fresh one
                    state = NEW_OPTION;
                    optionType = args[i];
                }
                else if (SUB_PARAM_OF_OPTION == state || NEW_OPTION == state)
                {
                    // new one but old one should be saved
                    commandLineOption = new CommandLineOption(optionType, optionBundle);
                    commandLineOptions.put(commandLineOption.getOptionType(), commandLineOption);
                    state = NEW_OPTION;
                    optionType = args[i];
                    optionBundle = null;

                }
            }
            else
            {
                if (NEW_OPTION == state)
                {
                    optionBundle = new ArrayList();
                    optionBundle.add(args[i]);
                    state = SUB_PARAM_OF_OPTION;

                }
                else if (SUB_PARAM_OF_OPTION == state)
                {
                    optionBundle.add(args[i]);
                }

            }
        }
        commandLineOption = new CommandLineOption(optionType, optionBundle);
        commandLineOptions.put(commandLineOption.getOptionType(), commandLineOption);
        return commandLineOptions;

    }

    public Map parsefirst(String[] args)
    {
        Map commandLineOptions = new HashMap();
        if (0 == args.length)
        {
            return commandLineOptions;
        }
        else if (1 == args.length)
        {
            return commandLineOptions;
        }
        /* when user is not giving the command line option with a "=" */
        // if (!args[2].startsWith("-"))
        // return commandLineOptions;
        // State 0 means started
        // State 1 means earlier one was a new -option
        // State 2 means earlier one was a sub param of a -option
        int state = STARTED;
        ArrayList optionBundle = null;
        String optionType = null;
        CommandLineOption commandLineOption;
        String newcommand = "";
        String[] newargs;
        int j;

        for (int i = 0; i < args.length; i++)
        {
            if (args[i].startsWith("-"))
            {
                if (STARTED == state)
                {
                    // fresh one
                    state = NEW_OPTION;
                    optionType = args[i];
                }
                else if (SUB_PARAM_OF_OPTION == state || NEW_OPTION == state)
                {
                    // new one but old one should be saved
                    commandLineOption = new CommandLineOption(optionType, optionBundle);
                    commandLineOptions.put(commandLineOption.getOptionType(), commandLineOption);
                    state = NEW_OPTION;
                    optionType = args[i];
                    optionBundle = null;

                }
            }
            else
            {
                if (NEW_OPTION == state)
                {
                    optionBundle = new ArrayList();
                    optionBundle.add(args[i]);
                    state = SUB_PARAM_OF_OPTION;

                }
                else if (SUB_PARAM_OF_OPTION == state)
                {
                    optionBundle.add(args[i]);
                }

            }
        }
        commandLineOption = new CommandLineOption(optionType, optionBundle);
        commandLineOptions.put(commandLineOption.getOptionType(), commandLineOption);
        return commandLineOptions;

    }

    public Map getAlloptions()
    {
        return this.commandlineoption;
    }

    public String getcommandname()
    {
        return this.commandname;
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
