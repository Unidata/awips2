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

import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;


/**
 * Field
 *
 */

public abstract class Field<C,T>
{

    private final Class<C> container;
    private final Class<T> type;
    private final String name;
    private final int index;

    Field(Class<C> container, Class<T> type, String name, int index)
    {
        this.container = container;
        this.type = type;
        this.name = name;
        this.index = index;
    }

    public final Class<C> getContainer()
    {
        return container;
    }

    public final Class<T> getType()
    {
        return type;
    }

    public final String getName()
    {
        return name;
    }

    public final int getIndex()
    {
        return index;
    }

    protected final C check(Object struct)
    {
        return container.cast(struct);
    }

    public abstract boolean has(Object struct);

    public abstract void has(Object struct, boolean value);

    public abstract T get(Object struct);

    public abstract void read(Decoder dec, Object struct);

    public abstract void write(Encoder enc, Object struct);

}
