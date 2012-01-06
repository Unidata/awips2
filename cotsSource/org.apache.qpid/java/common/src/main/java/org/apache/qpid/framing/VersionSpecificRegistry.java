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
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.framing.abstraction.ProtocolVersionMethodConverter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class VersionSpecificRegistry
{
    private static final Logger _log = LoggerFactory.getLogger(VersionSpecificRegistry.class);

    private final byte _protocolMajorVersion;
    private final byte _protocolMinorVersion;

    private static final int DEFAULT_MAX_CLASS_ID = 200;
    private static final int DEFAULT_MAX_METHOD_ID = 50;

    private AMQMethodBodyInstanceFactory[][] _registry = new AMQMethodBodyInstanceFactory[DEFAULT_MAX_CLASS_ID][];

    private ProtocolVersionMethodConverter _protocolVersionConverter;

    public VersionSpecificRegistry(byte major, byte minor)
    {
        _protocolMajorVersion = major;
        _protocolMinorVersion = minor;

        _protocolVersionConverter = loadProtocolVersionConverters(major, minor);
    }

    private static ProtocolVersionMethodConverter loadProtocolVersionConverters(byte protocolMajorVersion,
        byte protocolMinorVersion)
    {
        try
        {
            Class<ProtocolVersionMethodConverter> versionMethodConverterClass =
                (Class<ProtocolVersionMethodConverter>) Class.forName("org.apache.qpid.framing.MethodConverter_"
                    + protocolMajorVersion + "_" + protocolMinorVersion);

            return versionMethodConverterClass.newInstance();

        }
        catch (ClassNotFoundException e)
        {
            _log.warn("Could not find protocol conversion classes for " + protocolMajorVersion + "-" + protocolMinorVersion);
            if (protocolMinorVersion != 0)
            {
                protocolMinorVersion--;

                return loadProtocolVersionConverters(protocolMajorVersion, protocolMinorVersion);
            }
            else if (protocolMajorVersion != 0)
            {
                protocolMajorVersion--;

                return loadProtocolVersionConverters(protocolMajorVersion, protocolMinorVersion);
            }
            else
            {
                return null;
            }

        }
        catch (IllegalAccessException e)
        {
            throw new IllegalStateException("Unable to load protocol version converter: ", e);
        }
        catch (InstantiationException e)
        {
            throw new IllegalStateException("Unable to load protocol version converter: ", e);
        }
    }

    public byte getProtocolMajorVersion()
    {
        return _protocolMajorVersion;
    }

    public byte getProtocolMinorVersion()
    {
        return _protocolMinorVersion;
    }

    public AMQMethodBodyInstanceFactory getMethodBody(final short classID, final short methodID)
    {
        try
        {
            return _registry[classID][methodID];
        }
        catch (IndexOutOfBoundsException e)
        {
            return null;
        }
        catch (NullPointerException e)
        {
            return null;
        }
    }

    public void registerMethod(final short classID, final short methodID, final AMQMethodBodyInstanceFactory instanceFactory)
    {
        if (_registry.length <= classID)
        {
            AMQMethodBodyInstanceFactory[][] oldRegistry = _registry;
            _registry = new AMQMethodBodyInstanceFactory[classID + 1][];
            System.arraycopy(oldRegistry, 0, _registry, 0, oldRegistry.length);
        }

        if (_registry[classID] == null)
        {
            _registry[classID] =
                new AMQMethodBodyInstanceFactory[(methodID > DEFAULT_MAX_METHOD_ID) ? (methodID + 1)
                                                                                    : (DEFAULT_MAX_METHOD_ID + 1)];
        }
        else if (_registry[classID].length <= methodID)
        {
            AMQMethodBodyInstanceFactory[] oldMethods = _registry[classID];
            _registry[classID] = new AMQMethodBodyInstanceFactory[methodID + 1];
            System.arraycopy(oldMethods, 0, _registry[classID], 0, oldMethods.length);
        }

        _registry[classID][methodID] = instanceFactory;

    }

    public AMQMethodBody get(short classID, short methodID, ByteBuffer in, long size) throws AMQFrameDecodingException
    {
        AMQMethodBodyInstanceFactory bodyFactory;
        try
        {
            bodyFactory = _registry[classID][methodID];
        }
        catch (NullPointerException e)
        {
            throw new AMQFrameDecodingException(null, "Class " + classID + " unknown in AMQP version "
                + _protocolMajorVersion + "-" + _protocolMinorVersion + " (while trying to decode class " + classID
                + " method " + methodID + ".", e);
        }
        catch (IndexOutOfBoundsException e)
        {
            if (classID >= _registry.length)
            {
                throw new AMQFrameDecodingException(null, "Class " + classID + " unknown in AMQP version "
                    + _protocolMajorVersion + "-" + _protocolMinorVersion + " (while trying to decode class " + classID
                    + " method " + methodID + ".", e);

            }
            else
            {
                throw new AMQFrameDecodingException(null, "Method " + methodID + " unknown in AMQP version "
                    + _protocolMajorVersion + "-" + _protocolMinorVersion + " (while trying to decode class " + classID
                    + " method " + methodID + ".", e);

            }
        }

        if (bodyFactory == null)
        {
            throw new AMQFrameDecodingException(null, "Method " + methodID + " unknown in AMQP version "
                + _protocolMajorVersion + "-" + _protocolMinorVersion + " (while trying to decode class " + classID
                + " method " + methodID + ".", null);
        }

        return bodyFactory.newInstance( in, size);

    }

    public ProtocolVersionMethodConverter getProtocolVersionMethodConverter()
    {
        return _protocolVersionConverter;
    }

    public void configure()
    {
        _protocolVersionConverter.configure();
    }
}
