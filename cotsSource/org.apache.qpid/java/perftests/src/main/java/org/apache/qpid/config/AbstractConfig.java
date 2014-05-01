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
package org.apache.qpid.config;

public abstract class AbstractConfig
{
    public boolean setOptions(String[] argv)
    {
        try
        {
            for(int i = 0; i < argv.length - 1; i += 2)
            {
                String key = argv[i];
                String value = argv[i+1];
                setOption(key, value);
            }
            return true;
        }
        catch(Exception e)
        {
            System.out.println(e.getMessage());
            }
        return false;
    }
    
    protected int parseInt(String msg, String i)
    {
        try
        {
            return Integer.parseInt(i);
        }
        catch(NumberFormatException e)
        {
            throw new RuntimeException(msg + ": " + i, e);
        }
    }

    protected long parseLong(String msg, String i)
    {
        try
        {
            return Long.parseLong(i);
        }
        catch(NumberFormatException e)
        {
            throw new RuntimeException(msg + ": " + i, e);
        }
    }

    public abstract void setOption(String key, String value);
}
