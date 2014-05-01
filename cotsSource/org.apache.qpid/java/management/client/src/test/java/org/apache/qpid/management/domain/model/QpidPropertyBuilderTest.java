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

import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.access;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.index;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.max;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.min;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.optional;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.type;
import static org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute.unit;

import javax.management.MBeanAttributeInfo;

import org.apache.qpid.management.configuration.UnknownTypeCodeException;
import org.apache.qpid.management.domain.model.QpidFeatureBuilder.Attribute;

/**
 * Test case for Qpid Property builder.
 * 
 * @author Andrea Gazzarini
 */
public class QpidPropertyBuilderTest extends BaseQpidFeatureBuilderTestCase
{
    private final static Integer MIN = 0;
    private final static Integer MAX = 120;
    private final static String UNIT = "bytes";
    
    private Integer _access;
    
    @Override
    protected void setUp () throws Exception
    {
        super.setUp();

        _access = 1;
        _featureDefinition.put(access.name(), _access);
        _featureDefinition.put(unit.name(),UNIT);
        _featureDefinition.put(min.name(), MIN);
        _featureDefinition.put(max.name(),MAX);

        _featureDefinition.put(type.name(), 1);
        _featureDefinition.put(optional.name(),0);
        _featureDefinition.put(index.name(), 0);
        
        _builder = QpidFeatureBuilder.createPropertyBuilder(_featureDefinition);
    }

    /**
     * Tests the build process for a statistic when the definition map contains an unknown type code.
     * 
     * <br>precondition : the statistic definiton map contains an unknown type code.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testStatisticBuilderKO_WithUnknownType() 
    {
        int unknownTypeCode =999;
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
     * Tests the build process for a property when the definition map contains an invalid metadata type.
     * 
     * <br>precondition : the property definiton map contains a wrong type for a metadata attribute.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testPropertyBuilderKO_WithClassCastException() 
    {
        try
        {
            _featureDefinition.put(access.name(), new String("a"));
            _builder.build();
            fail("A wrong metadata attribute type should raise an exception to indicate a failure.");
        } catch (UnableToBuildFeatureException expected)
        {
            assertTrue(expected.getCause() instanceof ClassCastException);
        }
    }    
    
    /**
     * Tests the build process for a property when the definition map contains an unknown type code.
     * 
     * <br>precondition : the property definiton map contains an unknown type code.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testPropertyBuilderKO_WithUnknownType() 
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
     * Tests the build process for a property when the definition map doesn't contains type attribute.
     * 
     * <br>precondition: definition map doesn't contains type attribute.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attribute.
     */
    public void testPropertyBuilderKO_WithMissingType()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type);
    }

    /**
     * Tests the build process for a property when the definition map doesn't contain type & name attributes.
     * 
     * <br>precondition: definition map doesn't contain type & name attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testPropertyBuilderKO_WithMissingTypeAndName()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type, Attribute.name);
    }

    /**
     * Tests the build process for a property when the definition map doesn't contain type & name & index attributes.
     * 
     * <br>precondition: definition map doesn't contain type & name & index attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testPropertyBuilderKO_WithMissingTypeAndNameAndIndex()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type, Attribute.name,Attribute.index);
    }

    /**
     * Tests the build process for a property when the definition map doesn't contain type, name, index & optional attributes.
     * 
     * <br>precondition: definition map doesn't contain type, name, index & optional attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testPropertyBuilderKO_WithMissingTypeAndNameAndIndexAndOptional()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type, Attribute.name,Attribute.index,Attribute.optional);
    }

    /**
     * Tests the build process for a property when the definition map doesn't contain type, name, index, optional and access 
     * attributes.
     * 
     * <br>precondition: definition map doesn't contain type, name, index, optional and access attributes.
     * <br>postcondition : an exception should be thrown indicating the failure. That exception must contain the name of the 
     * missing attributes.
     */
    public void testPropertyBuilderKO_WithMissingTypeAndNameAndIndexAndOptionalAndAccess()
    {
        internalTestForMissingMandatoryAttribute(Attribute.type, Attribute.name,Attribute.index,Attribute.optional,Attribute.access);
    }
    
    /**
     * Tests the build process for a property when the definition map doesn't unit attribute.
     * Note that this attribute is optional and therefore the build must succeed.
     * 
     * <br>precondition: definition map doesn't contain unit attribute.
     * <br>postcondition : no exception is thrown and the property is built.
     */
    public void testBuilderOK_WithMissingUnit() throws UnableToBuildFeatureException
    {
        internalTestForMissingOptionalAttribute(Attribute.unit);  
    }

    /**
     * Tests the build process for a property when the definition map doesn't min and max attributes.
     * Note that those attributes are optional and therefore the build must succeed.
     * 
     * <br>precondition: definition map doesn't contain min and max attributes.
     * <br>postcondition : no exception is thrown and the property is built.
     */
    public void testBuilderOK_WithMissingMinAndMax() throws UnableToBuildFeatureException
    {
        internalTestForMissingOptionalAttribute(Attribute.min,Attribute.max);  
    }

    /**
     * Tests the build process for a property when the definition map doesn't description attribute.
     * Note that this attribute is optional and therefore the build must succeed.
     * 
     * <br>precondition: definition map doesn't contain description attribute.
     * <br>postcondition : no exception is thrown and the property is built.
     */
    public void testBuilderOK_WithMissingDescription() throws UnableToBuildFeatureException
    {
        internalTestForMissingOptionalAttribute(Attribute.desc);  
    }

    /**
     * Tests the build process for a property when the definition map contains valid values.
     * 
     * <br>precondition : the property definiton map contains valid values.
     * <br>postcondition : no exception is thrown and the property is built as expected.
     */
    public void testPropertyBuilderOK() throws UnableToBuildFeatureException 
    {
        _builder.build();
        
        QpidProperty property = (QpidProperty) _builder.getQpidFeature();
        MBeanAttributeInfo info = (MBeanAttributeInfo) _builder.getManagementFeature();
        
        assertEquals(NAME,property.getName());
        assertEquals(AccessMode.RC,property.getAccessMode());
        assertEquals(UNIT,property.getUnit());
        assertEquals(MIN.intValue(),property.getMinValue());
        assertEquals(MAX.intValue(),property.getMaxValue());
        assertEquals(Integer.MIN_VALUE,property.getMaxLength());
        assertEquals(DESCRIPTION,property.getDescription());
        assertEquals(Short.class,property.getJavaType());
        assertFalse(property.isOptional());
        
        assertEquals(property.getDescription(),info.getDescription());
        assertEquals(property.getName(),info.getName());
        assertEquals(property.getJavaType().getName(),info.getType());
    }
}
