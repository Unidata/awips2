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

import org.apache.qpid.management.TestConstants;

/**
 * Test case for Qpid package entity.
 * 
 * @author Andrea Gazzarini
 */
public class QpidPackageTest extends BaseDomainModelTestCase
{
    private QpidPackage _qpidPackage;
    
    @Override
    protected void setUp () throws Exception
    {
        _qpidPackage = new QpidPackage(TestConstants.QPID_PACKAGE_NAME, TestConstants.DOMAIN_MODEL);
    }
    
    /**
     * Tests the association of a new class with a qpid package.
     * 
     * <br>precondition : the package is not associated with any class.
     * <br>postcondition : the package is now associated with the given class.
     */
    public void testAddClass() throws UnableToBuildFeatureException {
        assertFalse(_qpidPackage.alreadyContainsClassDefinition(TestConstants.EXCHANGE_CLASS_NAME, TestConstants.HASH));
        
       _qpidPackage.getQpidClass(TestConstants.EXCHANGE_CLASS_NAME, TestConstants.HASH, true);
        
       assertTrue(_qpidPackage.alreadyContainsClassDefinition(TestConstants.EXCHANGE_CLASS_NAME, TestConstants.HASH));
    }
}