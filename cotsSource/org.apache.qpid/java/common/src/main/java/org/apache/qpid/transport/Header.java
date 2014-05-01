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
package org.apache.qpid.transport;

import org.apache.qpid.transport.network.Frame;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.LinkedHashMap;
import java.nio.ByteBuffer;


/**
 * Header
 *
 * @author Rafael H. Schloming
 */

public class Header {

    private final Struct[] structs;

    public Header(List<Struct> structs)
    {
        this(structs.toArray(new Struct[structs.size()]));
    }

    public Header(Struct ... structs)
    {
        this.structs = structs;
    }

    public Struct[] getStructs()
    {
        return structs;
    }


    public <T> T get(Class<T> klass)
    {
        for (Struct st : structs)
        {
            if (klass.isInstance(st))
            {
                return (T) st;
            }
        }

        return null;
    }

    public String toString()
    {
        StringBuffer str = new StringBuffer();
        str.append(" Header(");
        boolean first = true;
        for (Struct s : structs)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                str.append(", ");
            }
            str.append(s);
        }
        str.append(")");
        return str.toString();
    }

}
