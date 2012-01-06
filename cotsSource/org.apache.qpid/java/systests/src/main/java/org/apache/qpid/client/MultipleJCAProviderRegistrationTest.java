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
package org.apache.qpid.client;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.server.registry.ConfigurationFileApplicationRegistry;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.client.transport.TransportConnection;

import java.io.File;
import java.security.Provider;
import java.security.Security;
import java.util.List;
import java.util.LinkedList;

/**
 * QPID-1394 : Test to ensure that the client can register their custom JCAProviders after the broker to ensure that
 * the Qpid custom authentication SASL plugins are used.
 */
public class MultipleJCAProviderRegistrationTest extends QpidTestCase
{

    public void setUp() throws Exception
    {
        _broker = VM;

        super.setUp();
    }

    public void test() throws Exception
    {
        // Get the providers before connection
        Provider[] providers = Security.getProviders();

        // Force the client to load the providers
        getConnection();

        Provider[] afterConnectionCreation = Security.getProviders();

        // Find the additions
        List additions = new LinkedList();
        for (Provider afterCreation : afterConnectionCreation)
        {
            boolean found = false;
            for (Provider provider : providers)
            {
                if (provider == afterCreation)
                {
                    found=true;
                    break;
                }
            }

            // Record added registies
            if (!found)
            {
                additions.add(afterCreation);
            }
        }

        assertTrue("Client did not register any providers", additions.size() > 0);
    }

}
