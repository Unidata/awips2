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

import java.util.List;
import java.util.Map;

import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encodable;
import org.apache.qpid.transport.codec.Encoder;


/**
 * Struct
 *
 * @author Rafael H. Schloming
 */

public abstract class Struct implements Encodable
{

    public static Struct create(int type)
    {
        return StructFactory.create(type);
    }

    boolean dirty = true;

    public boolean isDirty()
    {
        return dirty;
    }

    public void setDirty(boolean dirty)
    {
        this.dirty = dirty;
    }

    public abstract int getStructType();

    public abstract int getSizeWidth();

    public abstract int getPackWidth();

    public final int getEncodedType()
    {
        int type = getStructType();
        if (type < 0)
        {
            throw new UnsupportedOperationException();
        }
        return type;
    }

    private final boolean isBit(Field<?,?> f)
    {
        return f.getType().equals(Boolean.class);
    }

    private final boolean packed()
    {
        return getPackWidth() > 0;
    }

    private final boolean encoded(Field<?,?> f)
    {
        return !packed() || !isBit(f) && f.has(this);
    }

    private final int getFlagWidth()
    {
        return (getFields().size() + 7)/8;
    }

    private final int getPaddWidth()
    {
        int pw = getPackWidth() - getFlagWidth();
        assert pw > 0;
        return pw;
    }

    private final int getFlagCount()
    {
        return 8*getPackWidth();
    }

    private final int getReservedFlagCount()
    {
        return getFlagCount() - getFields().size();
    }

    public abstract void read(Decoder dec);

    public abstract void write(Encoder enc);

    public abstract Map<String,Object> getFields();

    public String toString()
    {
        StringBuilder str = new StringBuilder();
        str.append(getClass().getSimpleName());

        str.append("(");
        boolean first = true;
        for (Map.Entry<String,Object> me : getFields().entrySet())
        {
            if (first)
            {
                first = false;
            }
            else
            {
                str.append(", ");
            }
            str.append(me.getKey());
            str.append("=");
            str.append(me.getValue());
        }
        str.append(")");

        return str.toString();
    }

}
