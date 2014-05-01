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

/**
 * Thrown when an attempt is made in order to update / change the state of an object and a constraint on that state 
 * is violated.
 * 
 * @author Andrea Gazzarini
 */
public class ValidationException extends Exception
{
    private static final long serialVersionUID = -5218828669655586205L;

    public final static String MAX_LENGTH = "Max Length";
    public final static String MAX_VALUE = "Max Value";
    public final static String MIN_VALUE = "Min Value";
    
    private final String _featureName;
    private final Object _featureValue;
    
    private final Number _constraintValue;
    private final String _constraintName;
    
    /**
     * Builds a new validation exception with the specified parameters.
     * 
     * @param constraintName the name of the violated constraint.
     * @param constraintValue the value of the violated constraint.
     * @param featureName the name of the violating feature.
     * @param featureValue the value of the violating feature.
     */
    ValidationException(String constraintName,Number constraintValue, String featureName,Object featureValue) 
    {
        super(String.format(
                            "Property constraint violation : " +
                            "%s allowed for property %s is %s but received value was %s",
                            constraintName,
                            featureName,
                            constraintValue,
                            featureValue));
        this._constraintName = constraintName;
        this._constraintValue = constraintValue;
        this._featureName = featureName;
        this._featureValue = featureValue;
    }

    /**
     * Returns the value of the violating feature.
     * 
     * @return the value  of the violating feature.
     */
    public Object getFeatureValue ()
    {
        return _featureValue;
    }
    
    /**
     * Returns the name of the violating feature.
     * 
     * @return the name of the violating feature.
     */
    public String  getFeatureName() 
    {
        return _featureName;
    }

    /**
     * Returns the value of the violated constraint.
     * 
     * @return the value of the violated constraint.
     */
    public Number getConstraintValue ()
    {
        return _constraintValue;
    }

    /**
     * Returns the name of the violated constraint.
     * 
     * @return the name of the violated constraint.
     */
    public String getConstraintName ()
    {
        return _constraintName;
    }
}