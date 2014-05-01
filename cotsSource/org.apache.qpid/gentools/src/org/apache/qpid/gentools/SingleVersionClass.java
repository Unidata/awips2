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
package org.apache.qpid.gentools;

import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map.Entry;


public class SingleVersionClass
{
    private final int _classId;


    private final AmqpClass _amqpClass;
    private final AmqpVersion _amqpVersion;
    private final Generator _generator;
    private final List<SingleVersionMethod> _methodList = new ArrayList<SingleVersionMethod>();

    public SingleVersionClass(AmqpClass amqpClass, AmqpVersion amqpVersion, Generator generator)
    {
        _amqpClass = amqpClass;
        _amqpVersion = amqpVersion;
        _generator = generator;

        AmqpOrdinalVersionMap indexMap = amqpClass.getIndexMap();
        int classId = 0;
        for(Entry<Integer, AmqpVersionSet> entry : indexMap.entrySet())
        {
            if(entry.getValue().contains(_amqpVersion))
            {
                classId = entry.getKey();
                break;
            }
        }
        _classId = classId;


        Collection<AmqpMethod> methods = _amqpClass.getMethodMap().values();

        for(AmqpMethod amqpMethod : methods)
        {
            _methodList.add(new SingleVersionMethod(amqpMethod, _amqpVersion, _generator));

        }

        Collections.sort(_methodList, new Comparator<SingleVersionMethod>(){
            public int compare(SingleVersionMethod method1, SingleVersionMethod method2)
            {
                return method1.getMethodId() - method2.getMethodId();
            }
        });


    }

    public int getClassId()
    {
        return _classId;
    }

    public String getName()
    {
        return _amqpClass.getName();
    }

    



    public List<SingleVersionMethod> getMethodList()
    {
        return _methodList;
    }


    public int getMaximumMethodId()
    {
        return _methodList.get(_methodList.size()-1).getMethodId();
    }
}
