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

import org.apache.qpid.management.configuration.Configurator;

import junit.framework.TestCase;

/**
 * Layer supertype for all domain model related test cases.
 * 
 * @author Andrea Gazzarini
 */
public abstract class BaseDomainModelTestCase extends TestCase
{
    /**
     * Set up fixture for this test case.
     * In order to execute tests on domain model we need to build the configuration.
     */
    @Override
    protected void setUp () throws Exception
    {
        Configurator configurator = new Configurator();
        configurator.configure();
    }
}