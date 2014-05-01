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

import java.nio.ByteBuffer;

import static org.apache.qpid.transport.util.Functions.*;


/**
 * Binary
 *
 */

public final class Binary
{

    private byte[] bytes;
    private int offset;
    private int size;
    private int hash = 0;

    public Binary(byte[] bytes, int offset, int size)
    {
        if (offset + size > bytes.length)
        {
            throw new ArrayIndexOutOfBoundsException();
        }

        this.bytes = bytes;
        this.offset = offset;
        this.size = size;
    }

    public Binary(byte[] bytes)
    {
        this(bytes, 0, bytes.length);
    }

    public final byte[] getBytes()
    {
        byte[] result = new byte[size];
        System.arraycopy(bytes, offset, result, 0, size);
        return result;
    }

    public final byte[] array()
    {
        return bytes;
    }

    public final int offset()
    {
        return offset;
    }

    public final int size()
    {
        return size;
    }

    public final Binary slice(int low, int high)
    {
        int sz;

        if (high < 0)
        {
            sz = size + high;
        }
        else
        {
            sz = high - low;
        }

        if (sz < 0)
        {
            sz = 0;
        }

        return new Binary(bytes, offset + low, sz);
    }

    public final int hashCode()
    {
        if (hash == 0)
        {
            int hc = 0;
            for (int i = 0; i < size; i++)
            {
                hc = 31*hc + (0xFF & bytes[offset + i]);
            }
            hash = hc;
        }

        return hash;
    }

    public final boolean equals(Object o)
    {
        if (!(o instanceof Binary))
        {
            return false;
        }

        Binary buf = (Binary) o;
        if (this.size != buf.size)
        {
            return false;
        }

        for (int i = 0; i < size; i++)
        {
            if (bytes[offset + i] != buf.bytes[buf.offset + i])
            {
                return false;
            }
        }

        return true;
    }

    public String toString()
    {
        return str(ByteBuffer.wrap(bytes, offset, size));
    }

}
