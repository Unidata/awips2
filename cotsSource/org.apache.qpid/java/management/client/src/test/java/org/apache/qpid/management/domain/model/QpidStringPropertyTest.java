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
import org.apache.qpid.management.domain.model.type.Str16;

public class QpidStringPropertyTest extends TestCase
{
    private QpidProperty _property;
    private final String _5LettersString = "12345";
    
    @Override
    protected void setUp () throws Exception
    {
        Configurator configurator = new Configurator();
        configurator.configure();
        _property = new QpidProperty();
        _property.setName("name");
        _property.setAccessMode(AccessMode.RW);
        _property.setType(new Str16());
    }
    
    /**
     * Tests the validation of a qpid property when the type is a string and a max length constraint hasn't been set.
     * 
     * <br>precondition : property type is a string, max length hasn't been set.
     * <br>postcondition : no exception is thrown. That is : the validation succeeds.
     */    
    public void testValidationWithoutMaxLength() {
        try
        {
            _property.validate(_5LettersString);
        } catch (ValidationException notExpected)
        {
            fail("No max length has been set on property so validation must succeed.");
        }
    }
    
    /**
     * Tests the validation of a qpid property when the type is a string and a max length constraint has been set.
     * 
     * <br>precondition : property type is a string, max length has been set and property value is longer than max length.
     * <br>postcondition : an exception is thrown indicating the validation failure.
     */
    public void testValidationKO_withMaxLength() {
        int maxLength = 2;
        _property.setMaxLength(maxLength);
        
        try
        {
            _property.validate(_5LettersString);
            fail("No max length has been set on property so validation must proceed.");
        } catch (ValidationException expected)
        {
            assertEquals(ValidationException.MAX_LENGTH,expected.getConstraintName());
            assertEquals(maxLength,expected.getConstraintValue());
            assertEquals(_5LettersString.length(),expected.getFeatureValue());
            assertEquals(_property.getName(),expected.getFeatureName());
        }
    }    
    
    /**
     * Tests the validation of a qpid property when the type is a string and the property value is null.
     * 
     * <br>precondition : property type is a string and property value is null..
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
        
        _property.setMaxLength(1);
        
        try
        {
            _property.validate(null);
        } catch (ValidationException notExpected)
        {
            fail("No constraint has been violated so validate() shouldn't raise an exception.");
        }
    }    
    
    /**
     * Tests the validation of a qpid property when the type is a string and a max length constraint has been set.
     * 
     * <br>precondition : property type is a string, max length has been set and property value is not violating that.
     * <br>postcondition : no exception is thrown. That is : the validation succeeds.
     */
    public void testValidationOK_withMaxLength() {
        int maxLength = (_5LettersString.length()+1);
        _property.setMaxLength(maxLength);
        
        try
        {
            _property.validate(_5LettersString);
        } catch (ValidationException notExpected)
        {
            fail("No constraint has been violated so validate() shouldn't raise an exception.");
        }
    }        
}
