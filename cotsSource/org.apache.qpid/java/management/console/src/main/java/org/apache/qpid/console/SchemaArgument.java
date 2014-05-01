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

import org.apache.qpid.transport.codec.Decoder;

public class SchemaArgument extends SchemaVariable
{
    private String direction;

    public SchemaArgument(Decoder dec, boolean methodArg)
    {
        Map<String, Object> map = dec.readMap();
        super.populateData(map);
        if (map.containsKey("dir"))
        {
            setDirection((String) map.get("dir"));
        }
    }

    public String getDirection()
    {
        return direction;
    }

    public boolean isBidirectional()
    {
        return getDirection().equals("IO");
    }

    public boolean isInput()
    {
        return getDirection().equals("I") | getDirection().equals("IO");
    }

    public boolean isOutput()
    {
        return getDirection().equals("O") | getDirection().equals("IO");
    }

    public void setDirection(String value)
    {
        direction = value;
    }
}