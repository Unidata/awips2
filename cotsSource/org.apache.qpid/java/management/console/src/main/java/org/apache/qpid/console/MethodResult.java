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

public class MethodResult
{
    private long returnCode;
    private String text;
    protected java.util.HashMap<String, Object> returnValues;

    public MethodResult(long aCode, String aMsg,
            java.util.HashMap<String, Object> args)
    {
        setReturnCode(aCode);
        setText(aMsg);
        returnValues = args;
    }

    public long getReturnCode()
    {
        return returnCode;
    }

    public Object getReturnValue(String name)
    {
        Object returnValue = null;
        if (returnValues.containsKey(name))
        {
            returnValue = returnValues.get(name);
        }
        return returnValue;
    }

    public HashMap<String, Object> getReturnValues()
    {
        return returnValues;
    }

    public String getText()
    {
        return text;
    }

    public void setReturnCode(long value)
    {
        returnCode = value;
    }

    public void setText(String value)
    {
        text = value;
    }

    @Override
    public String toString()
    {
        String returnString = "";
        for (java.util.Map.Entry<String, Object> pair : returnValues.entrySet())
        {
            returnString = returnString
                    + String.format("(Key: '%s' Value: '%s')", pair.getKey(),
                            pair.getValue());
        }
        return String.format(
                "MethodResult: ReturnCode=%s, Text=%s Values=[%s]",
                getReturnCode(), getText(), returnString);
    }
}