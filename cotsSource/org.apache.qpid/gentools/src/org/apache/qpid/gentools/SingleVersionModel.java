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

import java.util.Collection;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;


public class SingleVersionModel
{
    private final AmqpModel _amqpModel;
    private final AmqpVersion _amqpVersion;
    private final Generator _generator;
    private final List<SingleVersionClass> _classList = new ArrayList<SingleVersionClass>();

    public SingleVersionModel(AmqpModel amqpModel, AmqpVersion amqpVersion, Generator generator)
    {
        _amqpModel = amqpModel;
        _amqpVersion = amqpVersion;
        _generator = generator;


        Collection<AmqpClass> originalClasses = _amqpModel.getClassMap().values();

        for(AmqpClass amqpClass : originalClasses)
        {
            _classList.add(new SingleVersionClass(amqpClass, _amqpVersion, _generator));

        }

        Collections.sort(_classList, new Comparator<SingleVersionClass>(){
            public int compare(SingleVersionClass amqpClass1, SingleVersionClass amqpClass2)
            {
                return amqpClass1.getClassId() - amqpClass2.getClassId();
            }
        });


    }

    public Collection<SingleVersionClass> getClassList()
    {
        return Collections.unmodifiableCollection(_classList);
    }

    public int getMaximumClassId()
    {
        return _classList.get(_classList.size()-1).getClassId();
    }
}
