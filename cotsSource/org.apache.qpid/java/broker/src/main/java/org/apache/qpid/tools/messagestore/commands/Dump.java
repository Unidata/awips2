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
package org.apache.qpid.tools.messagestore.commands;

import org.apache.commons.codec.binary.Hex;
import org.apache.qpid.server.queue.QueueEntryImpl;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.tools.messagestore.MessageStoreTool;
import org.apache.qpid.tools.utils.Console;

import java.io.UnsupportedEncodingException;
import java.util.LinkedList;
import java.util.List;

public class Dump extends Show
{
    private static final int LINE_SIZE = 8;
    private static final String DEFAULT_ENCODING = "utf-8";
    private static final boolean SPACE_BYTES = true;
    private static final String BYTE_SPACER = " ";
    private static final String NON_PRINTING_ASCII_CHAR = "?";

    protected boolean _content = true;

    public Dump(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Dump selected message content. Default: show=content";
    }

    public String usage()
    {
        return getCommand() + " [show=[all],[msgheaders],[_amqHeaders],[routing],[content]] [id=<msgid e.g. 1,2,4-10>]";
    }

    public String getCommand()
    {
        return "dump";
    }

    public void execute(String... args)
    {
        assert args.length > 0;
        assert args[0].equals(getCommand());


        if (args.length >= 2)
        {
            for (String arg : args)
            {
                if (arg.startsWith("show="))
                {
                    _content = arg.contains("content") || arg.contains("all");
                }
            }

            parseArgs(args);
        }

        performShow();
    }


    protected List<List> createMessageData(java.util.List<Long> msgids, List<QueueEntry> messages, boolean showHeaders, boolean showRouting,
                                           boolean showMessageHeaders)
    {

        List<List> display = new LinkedList<List>();

        List<String> hex = new LinkedList<String>();
        List<String> ascii = new LinkedList<String>();
        display.add(hex);
        display.add(ascii);

        for (QueueEntry entry : messages)
        {
            ServerMessage msg = entry.getMessage();
            if (!includeMsg(msg, msgids))
            {
                continue;
            }

            //Add divider between messages
            hex.add(Console.ROW_DIVIDER);
            ascii.add(Console.ROW_DIVIDER);

            // Show general message information
            hex.add(Show.Columns.ID.name());
            ascii.add(msg.getMessageNumber().toString());

            hex.add(Console.ROW_DIVIDER);
            ascii.add(Console.ROW_DIVIDER);

            if (showRouting)
            {
                addShowInformation(hex, ascii, msg, "Routing Details", true, false, false);
            }
            if (showHeaders)
            {
                addShowInformation(hex, ascii, msg, "Headers", false, true, false);
            }
            if (showMessageHeaders)
            {
                addShowInformation(hex, ascii, msg, null, false, false, true);
            }

            // Add Content Body seciont
            hex.add("Content Body");
            ascii.add("");
            hex.add(Console.ROW_DIVIDER);
            ascii.add(Console.ROW_DIVIDER);


            final int messageSize = (int) msg.getSize();
            if (messageSize != 0)
            {
                hex.add("Hex");
                hex.add(Console.ROW_DIVIDER);


                ascii.add("ASCII");
                ascii.add(Console.ROW_DIVIDER);

                java.nio.ByteBuffer buf = java.nio.ByteBuffer.allocate(64 * 1024);

                int position = 0;

                while(position < messageSize)
                {

                    position += msg.getContent(buf, position);
                    buf.flip();
                    //Duplicate so we don't destroy original data :)
                    java.nio.ByteBuffer hexBuffer = buf;

                    java.nio.ByteBuffer charBuffer = hexBuffer.duplicate();

                    Hex hexencoder = new Hex();

                    while (hexBuffer.hasRemaining())
                    {
                        byte[] line = new byte[LINE_SIZE];

                        int bufsize = hexBuffer.remaining();
                        if (bufsize < LINE_SIZE)
                        {
                            hexBuffer.get(line, 0, bufsize);
                        }
                        else
                        {
                            bufsize = line.length;
                            hexBuffer.get(line);
                        }

                        byte[] encoded = hexencoder.encode(line);

                        try
                        {
                            String encStr = new String(encoded, 0, bufsize * 2, DEFAULT_ENCODING);
                            String hexLine = "";

                            int strKength = encStr.length();
                            for (int c = 0; c < strKength; c++)
                            {
                                hexLine += encStr.charAt(c);

                                if (c % 2 == 1 && SPACE_BYTES)
                                {
                                    hexLine += BYTE_SPACER;
                                }
                            }

                            hex.add(hexLine);
                        }
                        catch (UnsupportedEncodingException e)
                        {
                            _console.println(e.getMessage());
                            return null;
                        }
                    }

                    while (charBuffer.hasRemaining())
                    {
                        String asciiLine = "";

                        for (int pos = 0; pos < LINE_SIZE; pos++)
                        {
                            if (charBuffer.hasRemaining())
                            {
                                byte ch = charBuffer.get();

                                if (isPrintable(ch))
                                {
                                    asciiLine += (char) ch;
                                }
                                else
                                {
                                    asciiLine += NON_PRINTING_ASCII_CHAR;
                                }

                                if (SPACE_BYTES)
                                {
                                    asciiLine += BYTE_SPACER;
                                }
                            }
                            else
                            {
                                break;
                            }
                        }

                        ascii.add(asciiLine);
                    }
                    buf.clear();
                }
            }
            else
            {
                List<String> result = new LinkedList<String>();

                display.add(result);
                result.add("No ContentBodies");
            }
        }

        // if hex is empty then we have no data to display
        if (hex.size() == 0)
        {
            return null;
        }

        return display;
    }

    private void addShowInformation(List<String> column1, List<String> column2, ServerMessage msg,
                                    String title, boolean routing, boolean headers, boolean messageHeaders)
    {
        List<QueueEntry> single = new LinkedList<QueueEntry>();
        single.add(new QueueEntryImpl(null,msg, Long.MIN_VALUE));

        List<List> routingData = super.createMessageData(null, single, headers, routing, messageHeaders);

        //Reformat data
        if (title != null)
        {
            column1.add(title);
            column2.add("");
            column1.add(Console.ROW_DIVIDER);
            column2.add(Console.ROW_DIVIDER);
        }

        // look at all columns in the routing Data
        for (List item : routingData)
        {
            // the item should be:
            //   Title
            //   *divider
            //   value
            // otherwise we can't reason about the correct value
            if (item.size() == 3)
            {
                //Filter out the columns we are not interested in.

                String columnName = item.get(0).toString();

                if (!(columnName.equals(Show.Columns.ID.name())
                      || columnName.equals(Show.Columns.Size.name())))
                {
                    column1.add(columnName);
                    column2.add(item.get(2).toString());
                }
            }
        }
        column1.add(Console.ROW_DIVIDER);
        column2.add(Console.ROW_DIVIDER);
    }

    private boolean isPrintable(byte c)
    {
        return c > 31 && c < 127;
    }
}
