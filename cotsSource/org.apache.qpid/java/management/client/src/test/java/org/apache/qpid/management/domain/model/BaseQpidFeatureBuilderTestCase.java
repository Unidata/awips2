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

import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.desc;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.name;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.qpid.management.configuration.Configurator;
import org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute;

/**
 * Layer supertype for feature builder test cases.
 * 
 * @author Andrea Gazzarini
 */
public abstract class BaseQpidFeatureBuilderTestCase extends TestCase
{
    protected final static String NAME = "aName";

    protected final static String DESCRIPTION = "A description.";
    
    protected Map <String,Object> _featureDefinition;
    protected QpidFeatureBuilder _builder;

    /**
     * Set up fixture for all concrete builder test cases.
     */
    @Override
    protected void setUp () throws Exception
    {
        Configurator configurator = new Configurator();
        configurator.configure();
        _featureDefinition = new HashMap<String, Object>();
        _featureDefinition.put(name.name(),NAME);
        _featureDefinition.put(desc.name(), DESCRIPTION);
    }
    
    // Internal test used to avoid code duplication.
    protected void internalTestForMissingMandatoryAttribute(Attribute ...toBeRemoved) 
    {
        try
        {
            for (Attribute attribute : toBeRemoved)
            {
                _featureDefinition.remove(attribute.name());              
            }
            _builder.build();
            fail("If a mandatory attribute is missing an exception must be thrown!");
        } catch (UnableToBuildFeatureException expected)
        {
            assertTrue(expected instanceof MissingFeatureAttributesException);
            for (Attribute attribute : toBeRemoved)
            {
                assertTrue(expected.getMessage().contains(attribute.name()));
            }
        }
    }        

    // Internal test used to avoid code duplication.
    protected void internalTestForMissingOptionalAttribute(Attribute ...toBeRemoved) throws UnableToBuildFeatureException 
    {
        for (Attribute attribute : toBeRemoved)
        {
            _featureDefinition.remove(attribute.name());              
        }
        _builder.build();

        assertNotNull(_builder.getQpidFeature());
        assertNotNull(_builder.getManagementFeature());
    }                
    
    
}
