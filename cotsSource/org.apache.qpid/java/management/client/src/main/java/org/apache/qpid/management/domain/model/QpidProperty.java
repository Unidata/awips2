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

import java.lang.reflect.Constructor;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.domain.model.type.Type;
import org.apache.qpid.transport.util.Logger;

/**
 * Qpid property definition.
 * 
 * @author Andrea Gazzarini
 */
class QpidProperty extends QpidAttribute
{
    private final static Logger LOGGER = Logger.get(QpidProperty.class);
        
    private final static int [] MASKS  = {1,2,4,8,16,32,64,128};
    
    /**
     * Decoder interface used for decoding incomng values for this property.
     * 
     * @author Andrea Gazzarini
     */
    interface Decoder
    {
        Object decodeValue(org.apache.qpid.transport.codec.Decoder decoder,byte [] presenceBitMasks); 
    }
    
    /**
     * Decoder used for decoding incoming values for this optional property.
     */
    final Decoder _optionalPropertyDecoder = new Decoder() {

        public Object decodeValue (org.apache.qpid.transport.codec.Decoder decoder, byte[] presenceBitMasks)
        {
            return ((presenceBitMasks[_optionalIndex/8] &  MASKS[_maskIndex]) != 0) 
                ? QpidProperty.this.decodeValue(decoder) 
                : null;
        }
    };
    
    /**
     * Decoder used for decoding incoming values for this mandatory  property.
     */
    final Decoder _mandatoryPropertyDecoder = new Decoder() {

        public Object decodeValue (org.apache.qpid.transport.codec.Decoder decoder, byte[] presenceBitMasks)
        {
            return QpidProperty.this.decodeValue(decoder);
        }
    };
    
    
    /**
     * Null object used to perform a dummy validation.
     * This is the default validator installed at creation time.
     */
    final static IValidator EMPTY_VALIDATOR = new IValidator() 
    {
        public void validate (Object value) throws ValidationException
        {
            // Nothing to do here.
        }
    };
        
    /**
     * Validator responsible for validating strings.
     * At the moment the only constraint that should be applied to a string feature is the "max length"
     */
    class StringValidator implements IValidator 
    {
        public void validate (Object value) throws ValidationException
        {
            if ((_maxLength != Integer.MIN_VALUE) && (value != null)){
                int length = value.toString().length();
                if (length > _maxLength) {
                    throw new ValidationException(
                            ValidationException.MAX_LENGTH,
                            _maxLength, 
                            _name,
                            length);
                }
            }
        }
    };
    
    /**
     * Validator responsible for validating numbers.
     */
    class NumberValidator implements IValidator 
    {  
        public void validate (Object value) throws ValidationException
        {
            if (value != null) {
                double numericValue = ((Number)value).doubleValue();
                if (_minValue != Integer.MIN_VALUE && numericValue < _minValue) {
                    throw new ValidationException(
                            ValidationException.MIN_VALUE,
                            _minValue, 
                            _name,
                            numericValue);
                }

                if (_maxValue != Integer.MIN_VALUE && numericValue > _maxValue) {
                    throw new ValidationException(
                            ValidationException.MAX_VALUE,
                            _maxValue, 
                            _name,
                            numericValue);
                }
            }
        }
    };
    
    private AccessMode _accessMode;
    private int _minValue = Integer.MIN_VALUE;
    private int _maxValue = Integer.MIN_VALUE;
    private int _maxLength = Integer.MIN_VALUE;
    
    private int _optionalIndex;
    private int _maskIndex;
    
    Decoder _decoder = _mandatoryPropertyDecoder;
    
    private IValidator _validator = EMPTY_VALIDATOR;
    
    /**
     * Validates the given value according to the current validator.
     * It delegates the validation to the current installed validator.
     * 
     * @param value the value of this qpid property.
     * @throws ValidationException when the given value is violating the current validator constraints.
     */
    void validate(Object value) throws ValidationException {
        _validator.validate(value);
    }
    
    /**
     * Sets the type of this property.
     * In addition this method tries to detect if a validator has been associated with the type.
     * If no validator is found then the default validator will be used; that is : no validator will be performed on this 
     * property.
     * 
     * @param type the type of this property.
     */
    void setType (Type type)
    {
        super.setType(type);
        try {
            Class<?> validatorClass = Class.forName(Configuration.getInstance().getValidatorClassName(type));
            Constructor<?> validatorConstructor = validatorClass.getDeclaredConstructor(QpidProperty.class);
            _validator = (IValidator) validatorConstructor.newInstance(this);
            LOGGER.debug(Messages.QMAN_200022_VALIDATOR_INSTALLED ,validatorClass.getName(), type);
        } catch(Exception exception) {
            _validator = EMPTY_VALIDATOR;
            LOGGER.debug(Messages.QMAN_200023_VALIDATOR_NOT_FOUND , type);
        }
    }

    /**
     * Gets the value of this property according to its type definition.
     * 
     * @param decoder the decoder used to extract the value.
     * @return the value of this feature according to its type definition
     */
    Object decodeValue(org.apache.qpid.transport.codec.Decoder decoder,byte [] presenceBitMasks)
    {
        return _decoder.decodeValue(decoder, presenceBitMasks);
    }
    
    /**
     * Sets access mode for this property.
     * 
     * @param accessMode the access mode for this property.
     */
    void setAccessMode (AccessMode accessMode)
    {
        this._accessMode = accessMode;
    }

    /**
     * Gets the minimum allowed value for this property.
     * 
     * @return the minimum allowed value for this property.
     */
    int getMinValue ()
    {
        return _minValue;
    }

    /**
     * Sets the minimum allowed value for this property.
     * 
     * @param minValue the minimum allowed value for this property.
     */
    void setMinValue (int minValue)
    {
        this._minValue = minValue;
    }

    /**
     * Gets the maximum allowed value for this property.
     * 
     * @return the maximum  allowed value for this property.
     */
    int getMaxValue ()
    {
        return _maxValue;
    }

    /**
     * Sets the masimum allowed value for this property.
     * 
     * @param maxValue the maximum allowed value for this property.
     */
    void setMaxValue (int maxValue)
    {
        this._maxValue = maxValue;
    }

    /**
     * Gets the max length value for this property.
     * 
     * @return the max length value for this property.
     */    
    int getMaxLength ()
    {
        return _maxLength;
    }

    /**
     * Sets the max length value for this property.
     * 
     * @param maxLength the max length value for this property.
     */    
    void setMaxLength (int maxLength)
    {
        this._maxLength = maxLength;
    }
    
    /**
     * Gets the description of this property.
     * 
     * @return the description of this property.
     */
    AccessMode getAccessMode ()
    {
        return _accessMode;
    }

    /**
     * Marks this property as optional.
     * 
     * @param optional the optional attribute value for this property.
     * @param index the index of this optional property
     */
    void markAsOptional (int index)
    {
        this._optionalIndex = index;
        this._maskIndex = (_optionalIndex >= 8) ? _optionalIndex-8 : _optionalIndex;
        _decoder = _optionalPropertyDecoder;
    }

    /**
     * Returns true if this property is marked as optional.
     * 
     * @return true if this property is marked as optional, false otherwise.
     */
    boolean isOptional ()
    {
        return _decoder == _optionalPropertyDecoder;
    }
}
