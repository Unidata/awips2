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

import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.dir;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.name;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.type;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.unit;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.management.MBeanOperationInfo;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.handler.impl.MethodOrEventDataTransferObject;
import org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute;

/**
 * Test case for Qpid Statistic builder.
 * 
 * @author Andrea Gazzarini
 */
public class QpidMethodBuilderTest extends BaseQpidFeatureBuilderTestCase
{
    private final static Integer ARG_COUNT = 3;
    private MethodOrEventDataTransferObject _methodDefinition;
    
    private List<Map<String,Object>> _argumentsDefinitons = new ArrayList<Map<String, Object>>();
    
    @Override
    protected void setUp () throws Exception
    {  
        super.setUp();
        _featureDefinition.put(Names.ARG_COUNT_PARAM_NAME, ARG_COUNT);
        
        Map<String,Object> arg1 = new HashMap<String,Object>();
        arg1.put(name.name(), "arg1");
        arg1.put(type.name(),1);
        arg1.put(dir.name(),Direction.I.name());
        arg1.put(unit.name(), "bytes");
                
        Map<String,Object> arg2 = new HashMap<String,Object>();
        arg2.put(name.name(), "arg2");
        arg2.put(type.name(),1);
        arg2.put(dir.name(),Direction.O.name());
        arg2.put(unit.name(), "bytes");

        Map<String,Object> arg3 = new HashMap<String,Object>();
        arg3.put(name.name(), "arg3");
        arg3.put(type.name(),1);
        arg3.put(dir.name(),Direction.IO.name());
        arg3.put(unit.name(), "bytes");

        /*
        dir yes no  yes Direction code for method arguments
        unit    yes yes yes Units for numeric values (i.e. seconds, bytes, etc.)
        min yes no  yes Minimum value for numerics
        max yes no  yes Maximum value for numerics
        maxlen  yes no  yes Maximum length for strings
        desc    yes yes yes Description of the argument
        default yes no  yes Default value for the argument
        */
        _argumentsDefinitons.add(arg1);
        _argumentsDefinitons.add(arg2);
        
        _methodDefinition = new MethodOrEventDataTransferObject(_featureDefinition,_argumentsDefinitons);
        _builder = QpidFeatureBuilder.createMethodBuilder(_methodDefinition);
    }

    /**
     * Tests the build process for a statistic when the definition map doesn't contains type attribute.
     * 
     * <br>precondition: definition map doesn't contains type attribute.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attribute.
     */
    public void testMethodBuilderKO_WithMissingName()
    {
        internalTestForMissingMandatoryAttribute(Attribute.name);
    }

    /**
     * Tests the build process for a statistic when the definition map doesn't contain type, name, index & optional attributes.
     * 
     * <br>precondition: definition map doesn't contain type, name, index & optional attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testMethodBuilderOK_WithMissingUnit() throws UnableToBuildFeatureException
    {
        internalTestForMissingOptionalAttribute(Attribute.unit);
    }
    
    /**
     * Tests the build process for a statistic when the definition map doesn't unit attribute.
     * Note that this attribute is optional and therefore the build must succeed.
     * 
     * <br>precondition: definition map doesn't contain unit attribute.
     * <br>postcondition : no exception is thrown and the statistic is built.
     */
    public void testMethodBuilderOK_WithMissingDescription() throws UnableToBuildFeatureException
    {
        internalTestForMissingOptionalAttribute(Attribute.desc);  
    }

    /**
     * Tests the build process for a statistic when the definition map contains valid values.
     * 
     * <br>precondition : the statistic definiton map contains valid values.
     * <br>postcondition : no exception is thrown and the statistisc is built as expected.
     */
    public void testMethodBuilderOK() throws UnableToBuildFeatureException
    {
        _builder.build();
        
        QpidMethod method = (QpidMethod) _builder.getQpidFeature();
        MBeanOperationInfo info = (MBeanOperationInfo) _builder.getManagementFeature();
        
        assertEquals(NAME,method.getName());
       
        assertEquals(DESCRIPTION,method.getDescription());
        
        assertEquals(method.getDescription(),info.getDescription());
        assertEquals(method.getName(),info.getName());
    }
}
