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
package org.apache.qpid.management.domain.model;

import org.apache.qpid.management.Messages;
import org.apache.qpid.transport.codec.Encoder;
import org.apache.qpid.transport.util.Logger;

/**
 * An argument is the formal definition of a parameter belonging to a specific method / operation.
 */
class QpidArgument extends QpidProperty
{
    private final static Logger LOGGER = Logger.get(QpidArgument.class);
    
    private Object _defaultValue;
    
    private Direction _direction;
    
    /**
     * Sets the direction of this argument.
     * 
     * @param the direction of this argument.
     */
    public void setDirection(String code) 
    {
        this._direction = Direction.valueOf(code);
    }
    
    /**
     * Returns the direction of this argument.
     * 
     * @return the direction of this argument.
     */
    public Direction getDirection()
    {
        return _direction;
    }
    
    /**
     * Sets the default value of this argument.
     * 
     * @param defaultValue the default value of this argument.
     */
    public void setDefaultValue(Object defaultValue)
    {
        this._defaultValue = defaultValue;
    }

    /**
     * Returns the default value of this argument.
     * 
     * @return the default value of this argument.
     */
    public Object getDefaultValue()
    {
        return _defaultValue;
    }
    
    /**
     * Returns true if this is an Input argument.
     *  
     * @return true if this is an Input argument.
     */
    public boolean isInput()
    {
        return _direction != Direction.O;
    }
    
    @Override
    public String toString ()
    {
        return new StringBuilder()
            .append(getJavaType().getName())
            .append(' ')
            .append(_name)
            .append("(")
            .append(_direction)
            .append(")")
            .toString();
    }

    /**
     * Encodes the given value according to this argument type & definition.
     * 
     * @param value the value to be encoded.
     * @param encoder the encoder.
     */
    public void encode(Object value,Encoder encoder) 
    {
        _type.encode(value, encoder);
        LOGGER.debug(Messages.QMAN_200013_ARGUMENT_VALUE_ENCODED,value,_name,_type);
    }
    
    /**
     * Decodes the value for this argument according to its type & definition.
     * 
     * @param decoder the decoder
     * @return the decoded value of this argument.
     */
    public Object decode(org.apache.qpid.transport.codec.Decoder decoder) 
    {
        return _type.decode(decoder);
    }
}