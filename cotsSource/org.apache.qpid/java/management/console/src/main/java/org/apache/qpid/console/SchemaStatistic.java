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

import java.util.Map;

import org.apache.qpid.transport.codec.*;

public class SchemaStatistic
{
    private String description;
    private String name;
    private short type;
    private String unit;

    public SchemaStatistic(Decoder dec)
    {
        Map<String, Object> map = dec.readMap();
        setName((String) map.get("name"));
        setType(Short.parseShort("" + map.get("type")));
        if (map.containsKey("unit"))
        {
            setUnit((String) map.get("unit"));
        }
        if (map.containsKey("description"))
        {
            setDescription((String) map.get("description"));
        }
    }

    public String getDescription()
    {
        return description;
    }

    public String getName()
    {
        return name;
    }

    public short getType()
    {
        return type;
    }

    public String getUnit()
    {
        return unit;
    }

    public void setDescription(String value)
    {
        description = value;
    }

    public void setName(String value)
    {
        name = value;
    }

    public void setType(short value)
    {
        type = value;
    }

    public void setUnit(String value)
    {
        unit = value;
    }
}