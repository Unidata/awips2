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

public abstract class SchemaVariable
{
    private String defaultVariable;
    private String description;
    private Integer max;
    private Integer maxLength;
    private Integer min;
    private String name;
    private String refClass;
    private String refPackage;
    private short type;
    private String unit;

    public SchemaVariable()
    {
    }

    public String getDefault()
    {
        return defaultVariable;
    }

    public String getDescription()
    {
        return description;
    }

    public Integer getMax()
    {
        return max;
    }

    public Integer getMaxLength()
    {
        return maxLength;
    }

    public Integer getMin()
    {
        return min;
    }

    public String getName()
    {
        return name;
    }

    public String getRefClass()
    {
        return refClass;
    }

    public String getRefPackage()
    {
        return refPackage;
    }

    public short getType()
    {
        return type;
    }

    public String getUnit()
    {
        return unit;
    }

    protected void populateData(Map<String, Object> map)
    {
        if (map.containsKey("name"))
        {
            setName((String) map.get("name"));
        }
        if (map.containsKey("type"))
        {
            setType(Short.parseShort(("" + map.get("type"))));
        }
        if (map.containsKey("unit"))
        {
            setUnit((String) map.get("unit"));
        }
        if (map.containsKey("min"))
        {
            setMin((Integer) map.get("min"));
        }
        if (map.containsKey("max"))
        {
            setMax((Integer) map.get("max"));
        }
        if (map.containsKey("maxlen"))
        {
            setMaxLength((Integer) map.get("maxlen"));
        }
        if (map.containsKey("description"))
        {
            setDescription((String) map.get("description"));
        }
        if (map.containsKey("refClass"))
        {
            setRefClass((String) map.get("refClass"));
        }
        if (map.containsKey("refPackage"))
        {
            setRefPackage((String) map.get("refPackage"));
        }
        if (map.containsKey("Default"))
        {
            setDefault((String) map.get("default"));
        }
    }

    public void setDefault(String value)
    {
        defaultVariable = value;
    }

    public void setDescription(String value)
    {
        description = value;
    }

    public void setMax(Integer value)
    {
        max = value;
    }

    public void setMaxLength(Integer value)
    {
        maxLength = value;
    }

    public void setMin(Integer value)
    {
        min = value;
    }

    public void setName(String value)
    {
        name = value;
    }

    public void setRefClass(String value)
    {
        refClass = value;
    }

    public void setRefPackage(String value)
    {
        refPackage = value;
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