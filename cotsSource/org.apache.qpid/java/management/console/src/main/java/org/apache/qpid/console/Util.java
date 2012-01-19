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

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;

public class Util
{
    private static HashMap<Class, Short> ENCODINGS = new HashMap<Class, Short>();
    static
    {
        ENCODINGS.put(String.class, (short) 7);
        ENCODINGS.put(Short.class, (short) 1);
        ENCODINGS.put(Float.class, (short) 13);
        ENCODINGS.put(QMFObject.class, (short) 20);
        ENCODINGS.put(Integer.class, (short) 17);
        ENCODINGS.put(Long.class, (short) 18);
        ENCODINGS.put(ArrayList.class, (short) 21);
    }

    public static String accessName(int type)
    {
        switch (type)
        {
        // case 0: return "UNKNOWN" ;
        case 1:
            return "RC";
        case 2:
            return "RW";
        case 3:
            return "RO";
        }
        throw new ConsoleException(String.format("Invalid Access Code: %s",
                type));
    }

    public static String byteString(byte[] bytes)
    {
        return new String(bytes, Charset.forName("UTF-8"));
    }

    public static Object defaultValue(short type)
    {
        switch (type)
        {
        // case 0: return "UNKNOWN" ;
        case 1:
            return 0;
        case 2:
            return 0;
        case 3:
            return 0l;
        case 4:
            return 0l;
        case 5:
            return false;
        case 6:
            return "";
        case 7:
            return "";
        case 8:
            return 0l;
        case 9:
            return 0l;
        case 10:
            return new ObjectID();
        case 11:
            return false;
        case 12:
            return 0f;
        case 13:
            return 0d;
        case 14:
            return new UUID(0, 0);
        case 15:
            return new HashMap<String, Object>();
        case 16:
            return 0;
        case 17:
            return 0;
        case 18:
            return 0l;
        case 19:
            return 0l;
        case 20:
            return null;
        case 21:
            return new java.util.ArrayList<Object>();
        case 22:
            return new java.util.ArrayList<Object>();
        }
        throw new ConsoleException(String.format("Invalid Type Code: %s", type));
    }

    public static short qmfType(Object obj)
    {
        if (ENCODINGS.containsKey(obj.getClass()))
        {
            return ENCODINGS.get(obj.getClass());
        } else
        {
            throw new ConsoleException(String.format("Unkown Type of %s", obj
                    .getClass()));
        }
    }

    public static String typeName(short type)
    {
        switch (type)
        {
        // case 0: return "UNKNOWN" ;
        case 1:
            return "uint8";
        case 2:
            return "uint16";
        case 3:
            return "uint32";
        case 4:
            return "uint64";
        case 5:
            return "bool";
        case 6:
            return "short-string";
        case 7:
            return "long-string";
        case 8:
            return "abs-time";
        case 9:
            return "delta-time";
        case 10:
            return "reference";
        case 11:
            return "boolean";
        case 12:
            return "float";
        case 13:
            return "double";
        case 14:
            return "uuid";
        case 15:
            return "field-table";
        case 16:
            return "int8";
        case 17:
            return "int16";
        case 18:
            return "int32";
        case 19:
            return "int64";
        case 20:
            return "object";
        case 21:
            return "list";
        case 22:
            return "array";
        }
        throw new ConsoleException(String.format("Invalid Type Code: %s", type));
    }

    protected Util()
    {
    }
}