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
package org.apache.qpid.agent.binding;

import java.nio.ByteBuffer;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.codec.BBEncoder;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;

/**
 * Binding information from a java list to a QMF schema.
 */
public class ListBinding implements TypeBinding
{
    private static Log log = LogFactory.getLog(ListBinding.class);
    protected BindingContext bctx;
    protected Class javaClass;

    public ListBinding(BindingContext bctx, Class javaClass)
    {
        this.bctx = bctx;
        this.javaClass = javaClass;
    }

    public void encode(Encoder enc, Object value)
    {
        List list = (List) value;
        BBEncoder newEncoder = new BBEncoder(10);
        if (list != null)
        {
            newEncoder.writeUint32(list.size());
            for (Object obj : list)
            {
                TypeBinding type = bctx.getTypeBinding(obj.getClass());
                newEncoder.writeUint8(type.getCode());
                type.encode(newEncoder, obj);
            }
        } else
        {
            newEncoder.writeUint32(0);
        }
        enc.writeVbin32(newEncoder.buffer().array());
    }

    public Object decode(Decoder dec)
    {
        List list = null;
        try
        {
            list = (List) javaClass.newInstance();
        } catch (Exception e)
        {
            throw new BindingException(
                    "Could not create a List implementation for "
                            + javaClass.getName(), e);
        }
        BBDecoder newDecoder = new BBDecoder();
        newDecoder.init(ByteBuffer.wrap(dec.readVbin32()));
        long count = newDecoder.readUint32();
        while (count > 0)
        {
            short typeCode = newDecoder.readUint8();
            TypeBinding type = QMFTypeBinding.getType(typeCode);
            if (type == null)
            {
                type = bctx.getTypeBinding(Object.class);
            }
            list.add(type.decode(newDecoder));
            count -= 1;
        }
        return list;
    }

    // QMF List Type
    public short getCode()
    {
        return (short) 21;
    }

    @Override
    public Class<?> getJavaClass()
    {
        return javaClass;
    }

    @Override
    public String getRefClass()
    {
        return null;
    }

    @Override
    public String getRefPackage()
    {
        return null;
    }

    @Override
    public boolean isNative()
    {
        return true;
    }

    public boolean optionalDefault()
    {
        return false;
    }
}
