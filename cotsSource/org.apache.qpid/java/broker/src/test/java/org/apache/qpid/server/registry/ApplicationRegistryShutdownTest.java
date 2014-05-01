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
package org.apache.qpid.server.registry;

import junit.framework.TestCase;
import org.apache.qpid.server.util.TestApplicationRegistry;

import java.security.Security;
import java.security.Provider;
import java.util.List;
import java.util.LinkedList;

/**
 * QPID-1390 : Test to validate that the AuthenticationManger succesfully unregisters any new SASL providers when
 * The ApplicationRegistry is closed.
 *
 * This should be expanded as QPID-1399 is implemented.
 */
public class ApplicationRegistryShutdownTest extends TestCase
{

    IApplicationRegistry _registry;

    public void setUp() throws Exception
    {
        //Highlight that this test will cause a new AR to be created
        // This must used TestAppRegistry but during the test getInstance()
        // will be called so we must ensure to do the remove()
        _registry = new TestApplicationRegistry();
    }

    @Override
    public void tearDown() throws Exception
    {
        // Correctly Close the AR we created        
    	ApplicationRegistry.remove();
    }


    /**
     * QPID-1399 : Ensure that the Authentiction manager unregisters any SASL providers created during
     * ApplicationRegistry initialisation.
     * 
     */
    public void testAuthenticationMangerCleansUp()
    {
        // Get default providers
        Provider[] defaultProviders = Security.getProviders();

        // Register new providers
        try
        {
            _registry.initialise(ApplicationRegistry.DEFAULT_INSTANCE);
        }
        catch (Exception e)
        {
            fail(e.getMessage());
        }

        // Get the providers after initialisation
        Provider[] providersAfterInitialisation = Security.getProviders();

        // Find the additions
        List additions = new LinkedList();
        for (Provider afterInit : providersAfterInitialisation)
        {
            boolean found = false;
            for (Provider defaultProvider : defaultProviders)
            {
                if (defaultProvider == afterInit)
                {
                    found=true;
                    break;
                }
            }

            // Record added registies
            if (!found)
            {
                additions.add(afterInit);
            }
        }

        // Not using isEmpty as that is not in Java 5
        assertTrue("No new SASL mechanisms added by initialisation.", additions.size() != 0 );

        //Close the registry which will perform the close the AuthenticationManager
        try
        {
            _registry.close();
        }
        catch (Exception e)
        {
            fail(e.getMessage());
        }

        //Validate that the SASL plugins have been removed.
        Provider[] providersAfterClose = Security.getProviders();

        assertTrue("No providers unregistered", providersAfterInitialisation.length > providersAfterClose.length);

        //Ensure that the additions are not still present after close().
        for (Provider afterClose : providersAfterClose)
        {
            assertFalse("Added provider not unregistered", additions.contains(afterClose));
        }
    }





}
