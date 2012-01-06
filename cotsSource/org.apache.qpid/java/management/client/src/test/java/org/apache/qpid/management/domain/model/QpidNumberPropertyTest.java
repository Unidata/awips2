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

import junit.framework.TestCase;

import org.apache.qpid.management.configuration.Configurator;
import org.apache.qpid.management.domain.model.type.Uint8;

public class QpidNumberPropertyTest extends TestCase
{
    private QpidProperty _property;
    private Long _value = 55432L;
    
    @Override
    protected void setUp () throws Exception
    {
        Configurator configurator = new Configurator();
        configurator.configure();
        _property = new QpidProperty();
        _property.setName("average");
        _property.setAccessMode(AccessMode.RW);
        _property.setType(new Uint8());
    }
    
    /**
     * Tests the validation of a qpid property when the type is a number and no constraint has been set.
     * 
     * <br>precondition : property type is a string, no constraint has been set.
     * <br>postcondition : no exception is thrown and the validation succeeds.
     */    
    public void testValidationWithoutConstraints() {
        try
        {
            _property.validate(_value);
        } catch (ValidationException notExpected)
        {
            fail("If no constraint has been set on this property why the validation is failing?");
        }
    }
    
    /**
     * Tests the validation of a qpid property when the type is a number and a max value constraint has been set.
     * 
     * <br>precondition : property type is a number, max value has been set and property value is greater than max value.
     * <br>postcondition : an exception is thrown indicating the validation failure.
     */
    public void testValidationKO_withMaxValue() {
        int maxValue = (int)(_value-1);
        _property.setMaxValue(maxValue);
        
        try
        {
            _property.validate(_value);
            fail("The given value is violating the installed constraint so an exception must be thrown.");
        } catch (ValidationException expected)
        {
            assertEquals(ValidationException.MAX_VALUE,expected.getConstraintName());
            assertEquals(maxValue,expected.getConstraintValue());
            assertEquals((double)_value,expected.getFeatureValue());
            assertEquals(_property.getName(),expected.getFeatureName());
        }
    }    

    /**
     * Tests the validation of a qpid property when the type is a number and a min value constraint has been set.
     * 
     * <br>precondition : property type is a number, min value has been set and property value is lesser than min value.
     * <br>postcondition : an exception is thrown indicating the validation failure.
     */
    public void testValidationKO_withMinValue() {
        int minValue = (int)(_value+1);
        _property.setMinValue(minValue);
        
        try
        {
            _property.validate(_value);
            fail("The given value is violating the installed constraint so an exception must be thrown.");
        } catch (ValidationException expected)
        {
            assertEquals(ValidationException.MIN_VALUE,expected.getConstraintName());
            assertEquals(minValue,expected.getConstraintValue());
            assertEquals((double)_value,expected.getFeatureValue());
            assertEquals(_property.getName(),expected.getFeatureName());
        }
    }    
    
    
    /**
     * Tests the validation of a qpid property when the number is a string and the property value is null.
     * 
     * <br>precondition : property type is a number and property value is null..
     * <br>postcondition : no exception is thrown. That is : the validation succeeds.
     */
    public void testValidationOK_withNullValue() {        
        try
        {
            _property.validate(null);
        } catch (ValidationException notExpected)
        {
            fail("No constraint has been violated so validate() shouldn't raise an exception.");
        }
        
        _property.setMinValue(1);
        _property.setMaxValue(10);
        
        try
        {
            _property.validate(null);
        } catch (ValidationException notExpected)
        {
            fail("No constraint has been violated so validate() shouldn't raise an exception.");
        }
    }    
    
    /**
     * Tests the validation of a qpid property when the type is a number  and a max / min constraints have been set.
     * 
     * <br>precondition : property type is a number, max / min constraint have been set and property value is wrong.
     * <br>postcondition : an exception is thrown indicating the validation failure.
     */
    public void testValidationOK_withMinAndMaxConstraint() {
        int minValue = (int)(_value+1);
        int maxValue = (int)(_value-1);
        _property.setMinValue(minValue);
        _property.setMaxValue(maxValue);
        
        try
        {
            _property.validate(_value);
            fail("The given value is violating the installed constraint so an exception must be thrown.");
        } catch (ValidationException expected)
        {
            assertEquals(ValidationException.MIN_VALUE,expected.getConstraintName());
            assertEquals(minValue,expected.getConstraintValue());
            assertEquals((double)_value,expected.getFeatureValue());
            assertEquals(_property.getName(),expected.getFeatureName());
        }
        
        _property.setMinValue(0);
        try
        {
            _property.validate(_value);
            fail("The given value is violating the installed constraint so an exception must be thrown.");
        } catch (ValidationException expected)
        {
            assertEquals(ValidationException.MAX_VALUE,expected.getConstraintName());
            assertEquals(maxValue,expected.getConstraintValue());
            assertEquals((double)_value,expected.getFeatureValue());
            assertEquals(_property.getName(),expected.getFeatureName());
        }
    }        
}
