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
package org.apache.qpid.client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;

import org.apache.qpid.framing.AMQShortString;

public enum CustomJMSXProperty
{
    JMS_AMQP_NULL,
    JMS_QPID_DESTTYPE,
    JMSXGroupID,
    JMSXGroupSeq,
    JMSXUserID;


    private final AMQShortString _nameAsShortString;

    CustomJMSXProperty()
    {
        _nameAsShortString = new AMQShortString(toString());
    }

    public AMQShortString getShortStringName()
    {
        return _nameAsShortString;
    }

    private static Enumeration _names;

    public static synchronized Enumeration asEnumeration()
    {
        if(_names == null)
        {
            CustomJMSXProperty[] properties = values();
            ArrayList<String> nameList = new ArrayList<String>(properties.length);
            for(CustomJMSXProperty property :  properties)
            {
                nameList.add(property.toString());
            }
            _names = Collections.enumeration(nameList);
        }
        return _names;    
    }
}
