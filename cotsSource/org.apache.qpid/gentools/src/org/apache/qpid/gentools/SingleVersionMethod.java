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

import java.util.Map.Entry;
import java.util.Collection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class SingleVersionMethod
{
    private final AmqpMethod _amqpMethod;
    private final AmqpVersion _amqpVersion;
    private final int _methodId;
    private final List<SingleVersionField> _fieldList = new ArrayList<SingleVersionField>();
    private final Generator _generator;
    private final List<ConsolidatedField> _consolidatedFields = new ArrayList<ConsolidatedField>();
    private final Map<String, ConsolidatedField> _fieldNameToConsolidatedFieldMap = new HashMap<String, ConsolidatedField>();


    public SingleVersionMethod(AmqpMethod amqpMethod, AmqpVersion amqpVersion, Generator generator)
    {
        _amqpMethod = amqpMethod;
        _amqpVersion = amqpVersion;
        _generator = generator;

        AmqpOrdinalVersionMap indexMap = amqpMethod.getIndexMap();
        int methodId = 0;
        for(Entry<Integer, AmqpVersionSet> entry : indexMap.entrySet())
        {
            if(entry.getValue().contains(_amqpVersion))
            {
                methodId = entry.getKey();
                break;
            }
        }
        _methodId = methodId;

        Collection<AmqpField> fields = _amqpMethod.getFieldMap().values();

        for(AmqpField field : fields)
        {
            _fieldList.add(new SingleVersionField(field, _amqpVersion, _generator));

        }

        Collections.sort(_fieldList, new Comparator<SingleVersionField>(){
            public int compare(SingleVersionField field1, SingleVersionField field2)
            {
                return field1.getPosition() - field2.getPosition();
            }
        });



        ConsolidatedField lastField = null;
        int bitfieldNum = 0;
        for(SingleVersionField field : _fieldList)
        {
            String domainType = field.getDomainType();
            if(!domainType.equals("bit"))
            {
                lastField = new ConsolidatedField(_generator,
                                                  field.getName(),
                                                  field.getDomainType());
                _consolidatedFields.add(lastField);
            }
            else if(lastField == null || !lastField.getType().equals("bitfield"))
            {
                lastField = new ConsolidatedField(_generator,
                                                  domainType.equals("bit") ? "bitfield"+bitfieldNum++ : field.getName(),
                                                  domainType.equals("bit") ? "bitfield" : field.getDomainType(),
                                                  field.getName());
                _consolidatedFields.add(lastField);
            }
            else
            {
                lastField.add(field.getName());
            }
            _fieldNameToConsolidatedFieldMap.put(field.getName(), lastField);

        }
    }

    public int getMethodId()
    {
        return _methodId;
    }

    public String getName()
    {
        return _amqpMethod.getName();
    }

    public Collection<SingleVersionField> getFieldList()
    {
        return Collections.unmodifiableCollection(_fieldList);
    }

    public List<ConsolidatedField> getConsolidatedFields()
    {
        return _consolidatedFields;
    }

    public String getConsolidatedFieldName(String fieldName)
    {
        return _fieldNameToConsolidatedFieldMap.get(fieldName).getName();
    }

    public boolean isConsolidated(String fieldName)
    {
        return _fieldNameToConsolidatedFieldMap.get(fieldName).isConsolidated();
    }

    public int getPositionInBitField(String fieldName)
    {
        return _fieldNameToConsolidatedFieldMap.get(fieldName).getPosition(fieldName);
    }


    public boolean isServerMethod()
    {
        return _amqpMethod.isServerMethod(_amqpVersion);
    }


    public boolean isClientMethod()
    {
        return _amqpMethod.isClientMethod(_amqpVersion);        
    }

}
