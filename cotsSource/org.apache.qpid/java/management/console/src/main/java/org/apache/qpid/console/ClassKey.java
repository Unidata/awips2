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

import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ClassKey
{
    private static Logger log = LoggerFactory.getLogger(ClassKey.class);
    private String packageName;
    private String className;
    private long[] hash = new long[4];

    public ClassKey(Decoder dec)
    {
        setPackageName(dec.readStr8());
        setClassName(dec.readStr8());
        hash[0] = dec.readUint32();
        hash[1] = dec.readUint32();
        hash[2] = dec.readUint32();
        hash[3] = dec.readUint32();
    }

    public ClassKey(String keyString)
    {
        String delims = "[*:*(*)]";
        String[] parts = keyString.split(delims);
        if (parts.length < 3)
        {
            throw new ConsoleException(
                    "Invalid class key format. Format should be package:class(bytes)");
        }
        setPackageName(parts[0]);
        setClassName(parts[1]);
        delims = "-";
        String[] bytes = parts[2].split(delims);
        if (bytes.length != 4)
        {
            throw new ConsoleException(
                    "Invalid class key format. Bytes should be in the format HEX-HEX-HEX-HEX");
        }
        hash[0] = Long.parseLong(bytes[0], 16);
        hash[1] = Long.parseLong(bytes[1], 16);
        hash[2] = Long.parseLong(bytes[2], 16);
        hash[3] = Long.parseLong(bytes[3], 16);
    }

    public void encode(Encoder enc)
    {
        enc.writeStr8(getPackageName());
        enc.writeStr8(getClassName());
        enc.writeUint32(hash[0]);
        enc.writeUint32(hash[1]);
        enc.writeUint32(hash[2]);
        enc.writeUint32(hash[3]);
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj.getClass().equals(this.getClass()))
        {
            ClassKey other = (ClassKey) obj;
            return (other.getKeyString().equals(this.getKeyString()));
        } else
        {
            return false;
        }
    }

    public final String getClassName()
    {
        return className;
    }

    public long[] getHash()
    {
        return hash;
    }

    public String getHashString()
    {
        return String.format("%08x-%08x-%08x-%08x", hash[0], hash[1], hash[2],
                hash[3]);
    }

    public String getKeyString()
    {
        String hashString = this.getHashString();
        return String.format("%s:%s(%s)", getPackageName(), getClassName(),
                hashString);
    }

    public String getPackageName()
    {
        return packageName;
    }

    @Override
    public int hashCode()
    {
        return getKeyString().hashCode();
    }

    public void setClassName(String value)
    {
        className = value;
    }

    public void setHash(long[] hash)
    {
        this.hash = hash;
    }

    public void setPackageName(String value)
    {
        packageName = value;
    }

    @Override
    public String toString()
    {
        return String.format("ClassKey: %s", getKeyString());
    }
}