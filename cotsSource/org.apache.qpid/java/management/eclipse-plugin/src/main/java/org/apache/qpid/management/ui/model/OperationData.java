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
package org.apache.qpid.management.ui.model;

import java.util.List;

public class OperationData
{
    private String _name;
    private String _description;
    private String _returnType;
    private int    _impact;
    private List<ParameterData> _parameters;
 
    public OperationData(String value)
    {
        this._name = value;
    }
    
    public String getName()
    {
        return _name;
    }
    
    public String getDescription()
    {
        return _description;
    }
    
    public void setDescription(String description)
    {
        this._description = description;
    }

    public List<ParameterData> getParameters()
    {
        return _parameters;
    }

    public void setParameters(List<ParameterData> parameters)
    {
        this._parameters = parameters;
    }

    public int getImpact()
    {
        return _impact;
    }

    public void setImpact(int impact)
    {
        this._impact = impact;
    }

    public String getReturnType()
    {
        return _returnType;
    }

    public void setReturnType(String returnType)
    {
        this._returnType = returnType;
    }
    
    public boolean isReturnTypeBoolean()
    {
        return (_returnType.equals("boolean") || _returnType.equals("java.lang.Boolean"));
    }
    
    public boolean isReturnTypeVoid()
    {
        return (_returnType.equals("void") || _returnType.equals("java.lang.Void"));
    }
    
    public Object getParameterValue(String paramName)
    {
        if (_parameters == null)
        {
            return null;
        }
        
        for (int i = 0; i < _parameters.size(); i++)
        {
            if (paramName.equalsIgnoreCase(_parameters.get(i).getName()))
            {
                return _parameters.get(i).getValue();
            }
        }
        
        return null;
    }
}