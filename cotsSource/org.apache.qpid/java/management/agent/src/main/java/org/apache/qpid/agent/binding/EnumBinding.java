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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;

/**
 * Binding information from a java enum to a QMF schema
 */
public class EnumBinding extends ClassBinding
{
    private static Log log = LogFactory.getLog(EnumBinding.class);

    public EnumBinding(String pkg, String name, Class cls,
            boolean exposeBehaviour, BindingContext bctx)
    {
        super(pkg, name, cls, exposeBehaviour, bctx);
    }

    @Override
    public void encode(Encoder enc)
    {
        enc.writeUint8((short) 1); // kind
        enc.writeStr8(pkg);
        enc.writeStr8(name);
        enc.writeBin128(new byte[16]); // schema hash
        // FIXME Is there a way to send the valid types?
    }

    @Override
    public void encode(Encoder enc, Object value)
    {
        if (value != null)
        {
            enc.writeStr16(value.toString());
        } else
        {
            enc.writeStr16("");
        }
    }

    @Override
    public Object decode(Decoder dec)
    {
        // FIXME This only works with POJOs
        Object instance = null;
        String value = null;
        try
        {
            value = dec.readStr16();
            if ((value != null) && (!value.isEmpty()))
            {
                instance = Enum.valueOf((Class<Enum>) this.getJavaClass(),
                        value);
            }
        } catch (Exception e)
        {
            log.error(String.format(
                    "Could not create an enum of type %s with value %s",
                    this.javaClass.getName(), value));
            throw new BindingException(e);
        }
        return instance;
    }

    // Make this look like a String
    @Override
    public short getCode()
    {
        return (short) 7;
    }

    @Override
    public EnumBinding parse()
    {
        log.debug(String.format(
                "Parsing enum binding '%s' for package '%s' from class %s",
                name, pkg, javaClass.getName()));
        return this;
    }

    @Override
    public boolean optionalDefault()
    {
        return false;
    }
}
