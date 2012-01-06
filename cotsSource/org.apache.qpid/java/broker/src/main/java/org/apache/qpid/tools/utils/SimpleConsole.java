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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class SimpleConsole implements Console
{
    /** SLF4J Logger. */
    private static Logger _devlog = LoggerFactory.getLogger(SimpleConsole.class);

    /** Console Writer. */
    protected static BufferedWriter _consoleWriter;

    /** Console Reader. */
    protected static BufferedReader _consoleReader;

    /** Parser for command-line input. */
    protected CommandParser _parser;

    public SimpleConsole(BufferedWriter writer, BufferedReader reader)
    {
        _consoleWriter = writer;
        _consoleReader = reader;
        _parser = new SimpleCommandParser(_consoleReader);
    }

    public void print(String... message)
    {
        try
        {
            for (String s : message)
            {
                _consoleWriter.write(s);
            }
            _consoleWriter.flush();
        }
        catch (IOException e)
        {
            _devlog.error(e.getMessage() + ": Occured whilst trying to write:" + message);
        }

    }

    public void println(String... message)
    {
        print(message);
        print(System.getProperty("line.separator"));
    }


    public String readln()
    {
        try
        {
            return _consoleReader.readLine();
        }
        catch (IOException e)
        {
            _devlog.debug("Unable to read input due to:" + e.getMessage());
            return null;
        }
    }

    public String[] readCommand()
    {
        try
        {
            return _parser.parse();
        }
        catch (IOException e)
        {
            _devlog.error("Error reading command:" + e.getMessage());
            return new String[0];
        }
    }

    public CommandParser getCommandParser()
    {
        return _parser;
    }

    public void setCommandParser(CommandParser parser)
    {
        _parser = parser;
    }

    public void displayList(boolean hasTitle, String... list)
    {
        java.util.List<java.util.List> data = new LinkedList<List>();

        java.util.List<String> values = new LinkedList<String>();

        data.add(values);

        for (String value : list)
        {
            values.add(value);
        }

        if (hasTitle)
        {
            values.add(1, "*divider");
        }

        printMap(null, data);
    }

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
     *  |  Item 3     |  value 2     |   ..
     *  +----------------------------+
     *
     * @param title The title to display if any
     * @param entries the entries to display in a map.
     */
    public void printMap(String title, java.util.List<java.util.List> entries)
    {
        try
        {
            int columns = entries.size();

            int[] columnWidth = new int[columns];

            // calculate row count
            int rowMax = 0;

            //the longest item
            int itemMax = 0;

            for (int i = 0; i < columns; i++)
            {
                int columnIRowMax = entries.get(i).size();

                if (columnIRowMax > rowMax)
                {
                    rowMax = columnIRowMax;
                }
                for (Object values : entries.get(i))
                {
                    if (values.toString().equals(Console.ROW_DIVIDER))
                    {
                        continue;
                    }

                    int itemLength = values.toString().length();

                    //note for single width
                    if (itemLength > itemMax)
                    {
                        itemMax = itemLength;
                    }

                    //note for mulit width
                    if (itemLength > columnWidth[i])
                    {
                        columnWidth[i] = itemLength;
                    }

                }
            }

            int tableWidth = 0;


            for (int i = 0; i < columns; i++)
            {
                // plus 2 for the space padding
                columnWidth[i] += 2;
            }
            for (int size : columnWidth)
            {
                tableWidth += size;
            }
            tableWidth += (columns - 1);

            if (title != null)
            {
                if (title.length() > tableWidth)
                {
                    tableWidth = title.length();
                }

                printCellRow("+", "-", tableWidth);

                printCell(CellFormat.CENTRED, "|", tableWidth, " " + title + " ", 0);
                _consoleWriter.newLine();

            }

            //put top line | or bottom of title
            printCellRow("+", "-", tableWidth);

            //print the table data
            int row = 0;

            for (; row < rowMax; row++)
            {
                for (int i = 0; i < columns; i++)
                {
                    java.util.List columnData = entries.get(i);

                    String value;
                    // does this column have a value for this row
                    if (columnData.size() > row)
                    {
                        value = " " + columnData.get(row).toString() + " ";
                    }
                    else
                    {
                        value = "  ";
                    }

                    if (i == 0 && value.equals(" " + Console.ROW_DIVIDER + " "))
                    {
                        printCellRow("+", "-", tableWidth);
                        //move on to the next row
                        break;
                    }
                    else
                    {
                        printCell(CellFormat.LEFT, "|", columnWidth[i], value, i);
                    }

                    // if it is the last row then do a new line.
                    if (i == columns - 1)
                    {
                        _consoleWriter.newLine();
                    }
                }
            }

            printCellRow("+", "-", tableWidth);

        }
        catch (IOException e)
        {
            _devlog.error(e.getMessage() + ": Occured whilst trying to write.");
        }
    }

    public void close()
    {

        try
        {
            _consoleReader.close();
        }
        catch (IOException e)
        {
            _devlog.error(e.getMessage() + ": Occured whilst trying to close reader.");
        }

        try
        {

            _consoleWriter.close();
        }
        catch (IOException e)
        {
            _devlog.error(e.getMessage() + ": Occured whilst trying to close writer.");
        }

    }

    private void printCell(CellFormat format, String edge, int cellWidth, String cell, int column) throws IOException
    {
        int pad = cellWidth - cell.length();

        if (column == 0)
        {
            _consoleWriter.write(edge);
        }

        switch (format)
        {
            case CENTRED:
                printPad(" ", pad / 2);
                break;
            case RIGHT:
                printPad(" ", pad);
                break;
        }

        _consoleWriter.write(cell);


        switch (format)
        {
            case CENTRED:
                // if pad isn't even put the extra one on the right
                if (pad % 2 == 0)
                {
                    printPad(" ", pad / 2);
                }
                else
                {
                    printPad(" ", (pad / 2) + 1);
                }
                break;
            case LEFT:
                printPad(" ", pad);
                break;
        }


        _consoleWriter.write(edge);

    }

    private void printCellRow(String edge, String mid, int cellWidth) throws IOException
    {
        _consoleWriter.write(edge);

        printPad(mid, cellWidth);

        _consoleWriter.write(edge);
        _consoleWriter.newLine();
    }

    private void printPad(String padChar, int count) throws IOException
    {
        for (int i = 0; i < count; i++)
        {
            _consoleWriter.write(padChar);
        }
    }


}
