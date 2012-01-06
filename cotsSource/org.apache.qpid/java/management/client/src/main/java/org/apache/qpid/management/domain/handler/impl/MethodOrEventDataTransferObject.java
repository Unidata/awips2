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
package org.apache.qpid.management.domain.handler.impl;

import java.util.List;
import java.util.Map;

/**
 * Simple transfer object used for holding method / event definition data.
 * 
 * @author Andrea Gazzarini
 */
public class MethodOrEventDataTransferObject
{
    private final Map<String, Object> _definition;
    private List<Map<String, Object>> _argumentDefinitions;
    
    /**
     * Builds a new trasfer object with the given parameters.
     * 
     * @param definition the method definition.
     * @param argumentDefinitions the arguments definitions.
     */
    public MethodOrEventDataTransferObject(
            Map<String, Object> definition,
            List<Map<String, Object>> argumentDefinitions)
    {
        this._definition = definition;
        this._argumentDefinitions = argumentDefinitions;
    }
    
    /**
     * Returns the method definition.
     * 
     * @return the method definition.
     */
    public Map<String, Object> getDefinition() {
        return _definition;
    }
    
    /**
     * Returns the arguemnts definitions.
     * 
     * @return the arguemnts definitions.
     */
    public List<Map<String, Object>> getArgumentsDefinitions() 
    {
        return _argumentDefinitions;
    }
}