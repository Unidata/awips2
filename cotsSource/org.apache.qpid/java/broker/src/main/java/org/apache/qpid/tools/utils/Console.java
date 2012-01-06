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

import java.util.List;

public interface Console
{
    public enum CellFormat
    {
        CENTRED, LEFT, RIGHT
    }    

    public static String ROW_DIVIDER = "*divider";

    public void print(String... message);

    public void println(String... message);

    public String readln();

    /**
     * Reads and parses the command line.
     * 
     *
     * @return The next command or null
     */
    public String[] readCommand();

    public CommandParser getCommandParser();

    public void setCommandParser(CommandParser parser);

    /**
     *
     *  Prints the list of String nicely.
     *
     *  +-------------+
     *  |  Heading    |
     *  +-------------+
     *  |  Item 1     |
     *  |  Item 2     |
     *  |  Item 3     |
     *  +-------------+
     *
     * @param hasTitle should list[0] be used as a heading
     * @param list The list of Strings to display     
     */
    public void displayList(boolean hasTitle, String... list);

    /**
     *
     *  Prints the list of String nicely.
     *
     *  +----------------------------+
     *  |  Heading                   |
     *  +----------------------------+
     *  |  title      |  title       |   ..
     *  +----------------------------+
     *  |  Item 2     |  value 2     |   ..
     *  +----------------------------+ (*divider)
     *  |  Item 3     |  value 2     |   ..
     *  +----------------------------+
     *
     * @param title The title to display if any
     * @param entries the entries to display in a map.
     */
    void printMap(String title, List<List> entries);


    public void close();
}
