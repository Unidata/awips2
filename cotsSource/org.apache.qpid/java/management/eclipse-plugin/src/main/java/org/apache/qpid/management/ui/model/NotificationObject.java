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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.management.ObjectName;

import static org.apache.qpid.management.ui.Constants.VIRTUAL_HOST;

public class NotificationObject
{

    private long    _sequenceNo;
    private Date    _timeStamp;
    private String  _message;
    private Object  _source;       
    private String  _type;         // INFO, WARN, etc
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("hh:mm:ss dd/MM/yy z");
    
    public NotificationObject(long seqNo, Date timeStamp, String message, Object source, String type)
    {
        this._sequenceNo = seqNo;
        this._message = message;
        this._source  = source;
        this._type    = type;
        this._timeStamp = timeStamp;
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }
    
    public Object getSource()
    {
        return _source;
    }
    public void setSource(Object _source)
    {
        this._source = _source;
    }
    
    public String getSourceName()
    {
        if (_source instanceof ObjectName)
        {
            return ((ObjectName)_source).getKeyProperty("name");
        }
        
        return null;
    }
    
    public String getSourceVirtualHost()
    {
        if (_source instanceof ObjectName)
        {
            return ((ObjectName)_source).getKeyProperty(VIRTUAL_HOST);
        }
        
        return null;
    }
    
    public String getMessage()
    {
        return _message;
    }
    public void setMessage(String _message)
    {
        this._message = _message;
    }
    public long getSequenceNo()
    {
        return _sequenceNo;
    }
    public void setSequenceNo(long no)
    {
        _sequenceNo = no;
    }
    public String getTimeStamp()
    {
        return dateFormat.format(_timeStamp);
    }
    public void setTimeStamp(Date stamp)
    {
        _timeStamp = stamp;
    }
    public String getType()
    {
        return _type;
    }
    public void setType(String _type)
    {
        this._type = _type;
    }
       
}
