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
package org.apache.qpid.server.logging.rawloggers;

import org.apache.qpid.server.logging.RawMessageLogger;

import java.util.List;
import java.util.LinkedList;

public class UnitTestMessageLogger implements RawMessageLogger
{
    List<Object> _log;

    public UnitTestMessageLogger()
    {
        _log = new LinkedList<Object>();
    }


    public void rawMessage(String message)
    {
        _log.add(message);
    }

    public void rawMessage(String message, Throwable throwable)
    {
        _log.add(message);
        _log.add(throwable);
    }


    public List<Object> getLogMessages()
    {
        return _log;
    }

    public void clearLogMessages()
    {
        _log.clear();
    }

}
