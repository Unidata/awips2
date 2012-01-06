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
 * Thrown when a feature (property, statistic, method or event) definition cannot be built due to schema parsing errors.
 *  
 * @author Andrea Gazzarini
 */
public class UnableToBuildFeatureException extends Exception
{
    private static final long serialVersionUID = 5180111828887602836L;

    /**
     * Builds a new UnableToBuildFeatureException with the specified cause.
     * 
     * @param exception the exception cause.
     */
    UnableToBuildFeatureException(Exception exception, String featureName)
    {
        super( (featureName != null) ? featureName : "Feature name is not available for debugging purposes." ,exception);
    }
    
    /**
     * Builds a new UnableToBuildFeatureException with the specified message.
     * 
     * @param message the detail message.
     */
    UnableToBuildFeatureException(String message)
    {
        super(message);
    }    
}
