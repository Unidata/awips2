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
package org.apache.qpid.console;

import java.util.HashMap;

public class SequenceManager
{
    private long sequence = 0;
    private HashMap<Long, Object> pending = new HashMap<Long, Object>();
    private Object lockObject = new Object();

    public SequenceManager()
    {
    }

    public Object release(long seq)
    {
        Object returnValue = null;
        synchronized (lockObject)
        {
            returnValue = pending.get(seq);
            pending.remove(seq);
        }
        return returnValue;
    }

    public long reserve(Object data)
    {
        long returnValue = 0;
        synchronized (lockObject)
        {
            returnValue = sequence;
            sequence += 1;
            pending.put(returnValue, data);
        }
        return returnValue;
    }
}