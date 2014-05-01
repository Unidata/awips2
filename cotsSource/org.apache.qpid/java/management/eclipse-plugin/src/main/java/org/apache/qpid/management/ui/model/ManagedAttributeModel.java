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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ManagedAttributeModel
{
    HashMap<String, AttributeData> _attributeMap = new HashMap<String, AttributeData>();
    
    public void setAttributeValue(String name, Object value)
    {
        if (value == null)
            return;
        
        AttributeData data = null;
        String dataType = value.getClass().getName();
        if (_attributeMap.containsKey(name))
        {
            data = _attributeMap.get(name);
            data.setValue(value);
        }
        else
        {
            data = new AttributeData();
            data.setName(name);
            data.setValue(value);
            _attributeMap.put(name, data);
        }
        data.setDataType(dataType);
    }
    
    
    public void setAttributeDescription(String name, String value)
    {
        if (_attributeMap.containsKey(name))
        {
            _attributeMap.get(name).setDescription(value);
        }
        else
        {
            AttributeData data = new AttributeData();
            data.setName(name);
            data.setDescription(value);
            _attributeMap.put(name, data);
        }
    }
    
    public void setAttributeReadable(String name, boolean readable)
    {
        if (_attributeMap.containsKey(name))
        {
            _attributeMap.get(name).setReadable(readable);
        }
        else
        {
            AttributeData data = new AttributeData();
            data.setName(name);
            data.setReadable(readable);
            _attributeMap.put(name, data);
        }
    }
    
    public void setAttributeWritable(String name, boolean writable)
    {
        if (_attributeMap.containsKey(name))
        {
            _attributeMap.get(name).setWritable(writable);
        }
        else
        {
            AttributeData data = new AttributeData();
            data.setName(name);
            data.setWritable(writable);
            _attributeMap.put(name, data);
        }
    }
    
    public List<String> getAttributeNames()
    {
        return new ArrayList<String>(_attributeMap.keySet());
    }
    
    public AttributeData[] getAttributes()
    {
        return _attributeMap.values().toArray(new AttributeData[0]);
    }
    
    public AttributeData getAttribute(String name)
    {
        return _attributeMap.get(name);
    }
    
    public int getCount()
    {
        return _attributeMap.size();
    }
}
