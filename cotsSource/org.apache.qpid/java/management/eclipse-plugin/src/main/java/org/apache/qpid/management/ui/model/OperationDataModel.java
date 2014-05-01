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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;

public class OperationDataModel
{
    HashMap<String, OperationData> _operationMap = new HashMap<String, OperationData>();
    
    public void addOperation(MBeanOperationInfo opInfo)
    {
        OperationData opData = new OperationData(opInfo.getName());
        opData.setDescription(opInfo.getDescription());
        opData.setImpact(opInfo.getImpact());
        opData.setReturnType(opInfo.getReturnType());
        
        int parametersCount = opInfo.getSignature().length;
        if (parametersCount != 0)
        {
            List<ParameterData> paramList = new ArrayList<ParameterData>();
            for (int i = 0; i < parametersCount; i++)
            {
                MBeanParameterInfo paramInfo = opInfo.getSignature()[i];
                ParameterData param = new ParameterData(paramInfo.getName(), paramInfo.getDescription(),
                                                        paramInfo.getType());
                paramList.add(param);
            } 
            opData.setParameters(paramList);
        }
        
        _operationMap.put(opInfo.getName(), opData);
    }
    
    public OperationData getOperation(String name)
    {
        return _operationMap.get(name);
    }
    
    public List<OperationData> getOperations()
    {
        return new ArrayList<OperationData>(_operationMap.values());
    }
    
    public int getCount()
    {
        return _operationMap.size();
    }
}
