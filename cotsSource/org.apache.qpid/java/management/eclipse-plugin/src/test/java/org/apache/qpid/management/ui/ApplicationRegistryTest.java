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
package org.apache.qpid.management.ui;

import org.apache.qpid.management.common.mbeans.ServerInformation;

import junit.framework.TestCase;

public class ApplicationRegistryTest extends TestCase
{
    public void testSupportedManagementApiVersion()
    {
        //ensure that the console supported API version is kept in sync with the broker
        
        assertEquals("The management console does not support the same major version of management API as the broker. " +
                    "Make any required changes and update the supported value.",
        	     ServerInformation.QPID_JMX_API_MAJOR_VERSION,
                     ApplicationRegistry.SUPPORTED_QPID_JMX_API_MAJOR_VERSION);
        
        assertEquals("The management console does not support the same minor version of management API as the broker. " +
                    "Make any required changes and update the supported value.",
                    ServerInformation.QPID_JMX_API_MINOR_VERSION,
                    ApplicationRegistry.SUPPORTED_QPID_JMX_API_MINOR_VERSION);
    }
}
