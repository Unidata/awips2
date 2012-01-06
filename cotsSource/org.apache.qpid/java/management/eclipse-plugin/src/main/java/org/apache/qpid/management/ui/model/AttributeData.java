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
package org.apache.qpid.management.ui.model;

public class AttributeData
{
    String name = "";
    String description = "";
    String dataType = "";
    Object value = "";
    boolean readable = true;
    boolean writable = false;
    
    
    public String getDataType()
    {
        return dataType;
    }
    public void setDataType(String dataType)
    {
        this.dataType = dataType;
    }
    
    public String getDescription()
    {
        return description;
    }
    public void setDescription(String description)
    {
        this.description = description;
    }
    
    public String getName()
    {
        return name;
    }
    public void setName(String name)
    {
        this.name = name;
    } 
    
    public Object getValue()
    {
        return value;
    }
    public void setValue(Object value)
    {
        if (value != null)
            this.value = value;
    }
    public boolean isReadable()
    {
        return readable;
    }
    public void setReadable(boolean readable)
    {
        this.readable = readable;
    }
    public boolean isWritable()
    {
        return writable;
    }
    public void setWritable(boolean writable)
    {
        this.writable = writable;
    }
    
    public boolean isNumber()
    {
        if ("int".equals(dataType) || "java.lang.Integer".equals(dataType) ||
            "long".equals(dataType) || "java.lang.Long".equals(dataType) )
        {
            return true;
        }
        else
            return false;
    }
}
