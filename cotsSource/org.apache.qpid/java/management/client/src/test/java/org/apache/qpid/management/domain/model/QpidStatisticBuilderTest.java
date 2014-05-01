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

import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.type;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.unit;

import javax.management.MBeanAttributeInfo;

import org.apache.qpid.management.configuration.UnknownTypeCodeException;
import org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute;

/**
 * Test case for Qpid Statistic builder.
 * 
 * @author Andrea Gazzarini
 */
public class QpidStatisticBuilderTest extends BaseQpidFeatureBuilderTestCase
{
    private final static String UNIT = "bytes";
    
    @Override
    protected void setUp () throws Exception
    {  
        super.setUp();
        _featureDefinition.put(unit.name(),UNIT);
        _featureDefinition.put(type.name(), 1);
        
        _builder = QpidFeatureBuilder.createStatisticBuilder(_featureDefinition);
    }

    /**
     * Tests the build process for a statistic when the definition map contains an unknown type code.
     * 
     * <br>precondition : the statistic definiton map contains an unknown type code.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testStatisticBuilderKO_WithUnknownType() 
    {
        int unknownTypeCode = 999;
        try
        {
            _featureDefinition.put(type.name(), unknownTypeCode);
            _builder.build();
            fail("An unknown type code should raise an exception to indicate a failure.");
        } catch (UnableToBuildFeatureException expected)
        {
            assertEquals(unknownTypeCode,((UnknownTypeCodeException)expected.getCause()).getCode());
        }
    }
    
    /**
     * Tests the build process for a statistic when the definition map contains a null value for a metadata attribute.
     * 
     * <br>precondition : the statistic definiton map contains a null value for a metadata attribute.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testMethodBuilderKO_WithNullMetadataValue() 
    {
        try
        {
            _featureDefinition.put(type.name(), null);
            _builder.build();
            fail("An null value for a metadata attribute should raise an exception to indicate a failure.");
        } catch (UnableToBuildFeatureException expected)
        {
            assertTrue(expected.getCause() instanceof NullPointerException);
        }
    }        
    
    /**
     * Tests the build process for a statistic when the definition map doesn't contains type attribute.
     * 
     * <br>precondition: definition map doesn't contains type attribute.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attribute.
     */
    public void testStatisticBuilderKO_WithMissingType()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type);
    }

    /**
     * Tests the build process for a statistic when the definition map doesn't contain type & name attributes.
     * 
     * <br>precondition: definition map doesn't contain type & name attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testStatisticBuilderKO_WithMissingTypeAndName()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type, Attribute.name);
    }

    /**
     * Tests the build process for a statistic when the definition map doesn't contain type, name, index & optional attributes.
     * 
     * <br>precondition: definition map doesn't contain type, name, index & optional attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testStatisticBuilderOK_WithMissingUnit() throws UnableToBuildFeatureException
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
    public void testBuilderOK_WithMissingDescription() throws UnableToBuildFeatureException
    {
        internalTestForMissingOptionalAttribute(Attribute.desc);  
    }

    /**
     * Tests the build process for a statistic when the definition map contains valid values.
     * 
     * <br>precondition : the statistic definiton map contains valid values.
     * <br>postcondition : no exception is thrown and the statistisc is built as expected.
     */
    public void testStatisticBuilderOK() throws UnableToBuildFeatureException
    {
        _builder.build();
        
        QpidStatistic statistic= (QpidStatistic) _builder.getQpidFeature();
        MBeanAttributeInfo info = (MBeanAttributeInfo) _builder.getManagementFeature();
        
        assertEquals(NAME,statistic.getName());
        assertEquals(UNIT,statistic.getUnit());
        assertEquals(DESCRIPTION,statistic.getDescription());
        assertEquals(Short.class,statistic.getJavaType());
        
        assertEquals(statistic.getDescription(),info.getDescription());
        assertEquals(statistic.getName(),info.getName());
        assertEquals(statistic.getJavaType().getName(),info.getType());
    }
}
