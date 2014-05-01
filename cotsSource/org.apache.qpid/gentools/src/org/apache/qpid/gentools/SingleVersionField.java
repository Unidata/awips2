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
package org.apache.qpid.gentools;


public class SingleVersionField
{
    private final AmqpField _field;
    private final AmqpVersion _amqpVersion;
    private final Generator _generator;

    public SingleVersionField(AmqpField field, AmqpVersion amqpVersion, Generator generator)
    {
        _field = field;
        _amqpVersion = amqpVersion;
        _generator = generator;
    }

    public String getName()
    {
        return _field.getName();
    }

    public String getDomain()
    {
        return _field.getDomain(_amqpVersion);
    }


    public String getDomainType()
    {
        return _generator.getDomainType(_field.getDomain(_amqpVersion),_amqpVersion);
    }

    public String getNativeType()
    {
        return _generator.getNativeType(getDomainType());
    }

    public String getEncodingType()
    {
        return _generator.getEncodingType(getDomainType());
    }


    public int getPosition()
    {
        return _field.getOrdinal(_amqpVersion);
    }
}
