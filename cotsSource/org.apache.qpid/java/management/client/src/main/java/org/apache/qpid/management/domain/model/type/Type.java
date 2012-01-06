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
package org.apache.qpid.management.domain.model.type;

import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;

/**
 * Layer supertype for all management "types".
 * 
 * @author Andrea Gazzarini
 */
public abstract class Type
{
    /** Java representation of this type. */
    protected final Class<?> javaType;
    
    /**
     * Builds a new management type wiich wraps the given java type.
     * 
     * @param javaType the java type.
     */
    Type(Class<?> javaType)
    {
        this.javaType = javaType;
    }

    /**
     * Returns the wrapped java type.
     * 
     * @return the wrapped java type.
     */
    public Class<?> getJavaType ()
    {
        return javaType;
    }

    /**
     * Each concrete subclass must define here how to decode incoming data according.
     * 
     * @param decoder the decoder used to extract data.
     * @return the "typed" value.
     * 
     */
    public abstract Object decode(Decoder decoder);
    
    /**
     * Returns a string representation of this type.
     * 
     * @return a string representation of this type.
     */
    @Override
    public String toString ()
    {
        return new StringBuilder(getClass().getName())
            .append(" (wraps ")
            .append(javaType.getName())
            .append(')').toString();
    }
    
    /**
     * Identity for types is based on wrapped java type identity.
     */
    @Override
    public boolean equals (Object obj)
    {
        return getClass() == obj.getClass();
    }
    
    @Override
    public int hashCode ()
    {
        return getClass().hashCode();
    }

    /**
     * Encodes the given values according to this type definition.
     * 
     * @param value the value to be encoded.
     * @param encoder the encoder.
     */
    public abstract void encode (Object value,Encoder encoder);
}