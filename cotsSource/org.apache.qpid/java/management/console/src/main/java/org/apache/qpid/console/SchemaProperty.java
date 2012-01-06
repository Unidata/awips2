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

import java.util.Map;

import org.apache.qpid.transport.codec.*;

public class SchemaProperty extends SchemaVariable
{
    private int access;
    private boolean index;
    private boolean optional;

    public SchemaProperty(Decoder dec)
    {
        Map<String, Object> map = dec.readMap();
        super.populateData(map);
        setName((String) map.get("name"));
        if (map.containsKey("optional"))
        {
            setOptional((Integer) map.get("optional") != 0);
        }
        if (map.containsKey("index"))
        {
            setIndex((Integer) map.get("index") != 0);
        }
        if (map.containsKey("access"))
        {
            setAccess((Integer) map.get("access"));
        }
    }

    public int getAccess()
    {
        return access;
    }

    public boolean getIndex()
    {
        return index;
    }

    public boolean getOptional()
    {
        return optional;
    }

    public void setAccess(int value)
    {
        access = value;
    }

    public void setIndex(boolean value)
    {
        index = value;
    }

    public void setOptional(boolean value)
    {
        optional = value;
    }
}