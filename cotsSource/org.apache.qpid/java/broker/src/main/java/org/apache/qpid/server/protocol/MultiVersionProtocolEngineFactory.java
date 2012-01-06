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
package org.apache.qpid.server.protocol;

import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.IApplicationRegistry;

import java.util.Set;
import java.util.Arrays;
import java.util.HashSet;

public class MultiVersionProtocolEngineFactory implements ProtocolEngineFactory
{
    ;


    public enum VERSION { v0_8, v0_9, v0_9_1, v0_10 };

    private static final Set<VERSION> ALL_VERSIONS = new HashSet<VERSION>(Arrays.asList(VERSION.values()));

    private final IApplicationRegistry _appRegistry;
    private final String _fqdn;
    private final Set<VERSION> _supported;


    public MultiVersionProtocolEngineFactory()
    {
        this(1, "localhost", ALL_VERSIONS);
    }

    public MultiVersionProtocolEngineFactory(String fqdn, Set<VERSION> versions)
    {
        this(1, fqdn, versions);
    }


    public MultiVersionProtocolEngineFactory(String fqdn)
    {
        this(1, fqdn, ALL_VERSIONS);
    }

    public MultiVersionProtocolEngineFactory(int instance, String fqdn, Set<VERSION> supportedVersions)
    {
        _appRegistry = ApplicationRegistry.getInstance(instance);
        _fqdn = fqdn;
        _supported = supportedVersions;
    }


    public ProtocolEngine newProtocolEngine(NetworkDriver networkDriver)
    {
        return new MultiVersionProtocolEngine(_appRegistry, _fqdn, _supported, networkDriver);
    }
}
