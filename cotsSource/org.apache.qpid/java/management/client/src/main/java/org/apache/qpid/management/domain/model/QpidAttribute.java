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
import org.apache.qpid.management.domain.model.type.Type;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.util.Logger;

/**
 * Layer supertype for qpid properties and statistics. 
 * 
 * @author Andrea Gazzarini
 */
class QpidAttribute extends QpidFeature
{
    private final static Logger LOGGER = Logger.get(QpidAttribute.class);
    
    /** feature type */
    protected Type _type;
    
    /** feature unit */
    protected String _unit;

    /**
     * Returns the units used for numeric values (i.e. seconds, bytes, etc.)
     * 
     * @return the units used for numeric values (i.e. seconds, bytes, etc.)
     */
    String getUnit ()
    {
        return _unit;
    }

    /**
     * Sets the unit for this property.
     * 
     * @param unit the unit of this property.
     */
    void setUnit (String unit)
    {
        this._unit = unit;
    }
    
    /**
     * Returns the java type (class) of this feature.
     * 
     * @return the java type (class) of this feature.
     */
    Class<?> getJavaType ()
    {
        return _type.getJavaType();
    }

    /**
     * Sets the type of this feature.
     * 
     * @param type the type of this feature.
     */
    void setType (Type type)
    {
        this._type = type;
    }
    
    /**
     * Gets the value of this feature according to its type definition.
     * 
     * @param decoder the decoder used to extract the value.
     * @return the value of this feature according to its type definition
     */
    Object decodeValue(Decoder decoder)
    {
        try {
            return _type.decode(decoder);
        } catch(RuntimeException exception) 
        {
            LOGGER.error(exception,Messages.QMAN_100014_ATTRIBUTE_DECODING_FAILURE,this);
            throw exception;
        }
    }
    
    @Override
    public String toString ()
    {
        return super.toString()+",type="+_type;
    }
}
