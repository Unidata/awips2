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

/**
 * Class representing an mbean operation parameter
 * @author Bhupendra Bhardwaj
 */
public class ParameterData
{
    private String _name;
    private String _description;
    private String _type;
    private Object _value;
    
    ParameterData(String name, String desc, String type)
    {
        this._name = name;
        this._description = desc;
        this._type = type;
        setDefaultValue();
    }
    
    public String getDescription()
    {
        return _description;
    }
    
    public String getName()
    {
        return _name;
    }

    public String getType()
    {
        return _type;
    }

    public Object getValue()
    {
        return _value;
    }
    
    public void setValueFromString(String strValue)
    {
        if ("int".equals(_type))
            _value = Integer.parseInt(strValue);
        else if (isBoolean())
            _value = Boolean.valueOf(strValue);
        else if ("long".equals(_type))
            _value = Long.parseLong(strValue);
        else
            _value = strValue; 
    }
    
    public void setValue(Object value)
    {
        this._value = value;
    }
    
    public boolean isBoolean()
    {
        return (_type.equals("boolean") || _type.equals("java.lang.Boolean"));
    }
    
    public void setDefaultValue()
    {
        if (isBoolean())
        {
            _value = Boolean.valueOf("false");
        }
        else
        {
            _value = null;
        }
    }
}
