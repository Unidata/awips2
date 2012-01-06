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
package org.apache.qpid.tools.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;

public class SimpleCommandParser implements CommandParser
{
    private static final String COMMAND_SEPERATOR = ";";

    /** Input source of commands */
    protected BufferedReader _reader;

    /** The next list of commands from the command line */
    private StringBuilder _nextCommand = null;

    public SimpleCommandParser(BufferedReader reader)
    {
        _reader = reader;
    }

    public boolean more()
    {
        return _nextCommand != null;
    }

    public boolean isBackground()
    {
        return false;
    }

    public String[] parse() throws IOException
    {
        String[] commands = null;

        String input = null;

        if (_nextCommand == null)
        {
            input = _reader.readLine();
        }
        else
        {
            input = _nextCommand.toString();
            _nextCommand = null;
        }

        if (input == null)
        {
            return null;
        }

        StringTokenizer tok = new StringTokenizer(input, " ");

        int tokenCount = tok.countTokens();
        int index = 0;

        if (tokenCount > 0)
        {
            commands = new String[tokenCount];
            boolean commandComplete = false;

            while (tok.hasMoreTokens())
            {
                String next = tok.nextToken();

                if (next.equals(COMMAND_SEPERATOR))
                {
                    commandComplete = true;
                    _nextCommand = new StringBuilder();
                    continue;
                }

                if (commandComplete)
                {
                    _nextCommand.append(next);
                    _nextCommand.append(" ");
                }
                else
                {
                    commands[index] = next;
                    index++;
                }
            }

        }

        //Reduce the String[] if not all the tokens were used in this command.
        // i.e. there is more than one command on the line.
        if (index != tokenCount)
        {
            String[] shortCommands = new String[index];
            System.arraycopy(commands, 0, shortCommands, 0, index);
            return shortCommands;
        }
        else
        {
            return commands;
        }
    }
}
