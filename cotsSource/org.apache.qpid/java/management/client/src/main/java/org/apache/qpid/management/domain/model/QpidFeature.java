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
 * Layer Supertype for all qpid management features.
 */
abstract class QpidFeature
{    
    /** The name of the feature. */
    protected String _name;
    
    /**
     * The description of the feature.
     */
    protected String _description;
    
    /**
     * Returns the description of this feature.
     * 
     * @return the description of this feature.
     */
    String getDescription ()
    {
        return _description;
    }
    
    /**
     * Sets the description for this feature.
     * 
     * @param description the description for this feature.
     */
    void setDescription (String description)
    {
        this._description = description;
    }
    
    /**
     * Returns the name of the feature.
     * 
     * @return the name of the feature.
     */
    public String getName ()
    {
        return _name;
    }

    /**
     * Sets the name for this feature.
     * 
     * @param name the name of this feature.
     */
    void setName (String name)
    {
        this._name = name;
    }  
    
    /**
     * Returns the name of the feature.
     * 
     * @return the name of the feature.
     */
    @Override
    public String toString ()
    {
        return _name;
    }
}