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
package org.apache.qpid.console;//

import java.util.ArrayList;
import java.util.Map;

import org.apache.qpid.transport.codec.*;

public class SchemaMethod
{
    public ArrayList<SchemaArgument> Arguments = new ArrayList<SchemaArgument>();
    private int m_ArgCount;
    private int m_BidirectionalArgCount;
    private String m_Description;
    private int m_InputArgCount;
    private String m_Name;
    private int m_OutputArgCount;

    public SchemaMethod(Decoder dec)
    {
        Map<String, Object> map = dec.readMap();
        setName((String) map.get("name"));
        setArgCount((Integer) map.get("argCount"));
        if (map.containsKey("desc"))
        {
            setDescription((String) map.get("desc"));
        }
        for (int x = 0; x < getArgCount(); x++)
        {
            SchemaArgument arg = new SchemaArgument(dec, true);
            Arguments.add(arg);
            if (arg.isInput())
            {
                setInputArgCount(getInputArgCount() + 1);
            }
            if (arg.isOutput())
            {
                setOutputArgCount(getOutputArgCount() + 1);
            }
            if (arg.isBidirectional())
            {
                setBidirectionalArgCount(getBidirectionalArgCount() + 1);
            }
        }
    }

    public final int getArgCount()
    {
        return m_ArgCount;
    }

    public final int getBidirectionalArgCount()
    {
        return m_BidirectionalArgCount;
    }

    public final String getDescription()
    {
        return m_Description;
    }

    public final int getInputArgCount()
    {
        return m_InputArgCount;
    }

    public final String getName()
    {
        return m_Name;
    }

    public final int getOutputArgCount()
    {
        return m_OutputArgCount;
    }

    public final void setArgCount(int value)
    {
        m_ArgCount = value;
    }

    public final void setBidirectionalArgCount(int value)
    {
        m_BidirectionalArgCount = value;
    }

    public final void setDescription(String value)
    {
        m_Description = value;
    }

    public final void setInputArgCount(int value)
    {
        m_InputArgCount = value;
    }

    public final void setName(String value)
    {
        m_Name = value;
    }

    public final void setOutputArgCount(int value)
    {
        m_OutputArgCount = value;
    }
}