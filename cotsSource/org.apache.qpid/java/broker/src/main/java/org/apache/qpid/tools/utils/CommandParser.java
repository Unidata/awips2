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

public interface CommandParser
{
    /**
     * If there is more than one command received on the last parse request.
     *
     * Subsequent calls to parse will utilise this input rather than reading new data from the input source
     * @return boolean
     */
    boolean more();

    /**
     * True if the currently parsed command has been requested as a background operation
     *
     * @return boolean
     */
    boolean isBackground();

    /**
     * Parses user commands, and groups tokens in the
     * String[] format that all Java main's love.
     *
     * If more than one command is provided in one input line then the more() method will return true.
     * A subsequent call to parse() will continue to parse that input line before reading new input.
     *
     * @return <code>input</code> split in args[] format; null if eof.
     * @throws java.io.IOException if there is a problem reading from the input stream
     */
    String[] parse() throws java.io.IOException;
}
