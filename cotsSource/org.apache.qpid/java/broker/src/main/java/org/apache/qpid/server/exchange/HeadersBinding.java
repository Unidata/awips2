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
package org.apache.qpid.server.exchange;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.apache.qpid.framing.AMQTypedValue;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.message.AMQMessageHeader;

/**
 * Defines binding and matching based on a set of headers.
 */
class HeadersBinding
{
    private static final Logger _logger = Logger.getLogger(HeadersBinding.class);

    private final FieldTable _mappings;
    private final Set<String> required = new HashSet<String>();
    private final Map<String,Object> matches = new HashMap<String,Object>();
    private boolean matchAny;

    private final class MatchesOrProcessor implements FieldTable.FieldTableElementProcessor
    {
        private Boolean _result = Boolean.FALSE;

        public boolean processElement(String propertyName, AMQTypedValue value)
        {
            if((value != null) && (value.getValue() != null) && value.getValue().equals(matches.get(propertyName)))
            {
                _result = Boolean.TRUE;
                return false;
            }
            return true;
        }

        public Object getResult()
        {
            return _result;
        }
    }

    private final class RequiredOrProcessor implements FieldTable.FieldTableElementProcessor
    {
        Boolean _result = Boolean.FALSE;

        public boolean processElement(String propertyName, AMQTypedValue value)
        {
            if(required.contains(propertyName))
            {
                _result = Boolean.TRUE;
                return false;
            }
            return true;
        }

        public Object getResult()
        {
            return _result;
        }
    }



    /**
     * Creates a binding for a set of mappings. Those mappings whose value is
     * null or the empty string are assumed only to be required headers, with
     * no constraint on the value. Those with a non-null value are assumed to
     * define a required match of value.
     * @param mappings the defined mappings this binding should use
     */

    HeadersBinding(FieldTable mappings)
    {
        _mappings = mappings;
        initMappings();
    }

    private void initMappings()
    {

        _mappings.processOverElements(new FieldTable.FieldTableElementProcessor()
        {

            public boolean processElement(String propertyName, AMQTypedValue value)
            {
                if (isSpecial(propertyName))
                {
                    processSpecial(propertyName, value.getValue());
                }
                else if (value.getValue() == null || value.getValue().equals(""))
                {
                    required.add(propertyName);
                }
                else
                {
                    matches.put(propertyName,value.getValue());
                }

                return true;
            }

            public Object getResult()
            {
                return null;
            }
        });
    }

    protected FieldTable getMappings()
    {
        return _mappings;
    }

    /**
     * Checks whether the supplied headers match the requirements of this binding
     * @param headers the headers to check
     * @return true if the headers define any required keys and match any required
     * values
     */
    public boolean matches(AMQMessageHeader headers)
    {
        if(headers == null)
        {
            return required.isEmpty() && matches.isEmpty();
        }
        else
        {
            return matchAny ? or(headers) : and(headers);
        }
    }

    private boolean and(AMQMessageHeader headers)
    {
        if(headers.containsHeaders(required))
        {
            for(Map.Entry<String, Object> e : matches.entrySet())
            {
                if(!e.getValue().equals(headers.getHeader(e.getKey())))
                {
                    return false;
                }
            }
            return true;
        }
        else
        {
            return false;
        }
    }


    private boolean or(final AMQMessageHeader headers)
    {
        if(required.isEmpty())
        {
            return  matches.isEmpty() || passesMatchesOr(headers);
        }
        else
        {
            if(!passesRequiredOr(headers))
            {
                return !matches.isEmpty() && passesMatchesOr(headers);
            }
            else
            {
                return true;
            }

        }
    }

    private boolean passesMatchesOr(AMQMessageHeader headers)
    {
        for(Map.Entry<String,Object> entry : matches.entrySet())
        {
            if(headers.containsHeader(entry.getKey())
               && ((entry.getValue() == null && headers.getHeader(entry.getKey()) == null)
                   || (entry.getValue().equals(headers.getHeader(entry.getKey())))))
            {
                return true;
            }
        }
        return false;
    }

    private boolean passesRequiredOr(AMQMessageHeader headers)
    {
        for(String name : required)
        {
            if(headers.containsHeader(name))
            {
                return true;
            }
        }
        return false;
    }

    private void processSpecial(String key, Object value)
    {
        if("X-match".equalsIgnoreCase(key))
        {
            matchAny = isAny(value);
        }
        else
        {
            _logger.warn("Ignoring special header: " + key);
        }
    }

    private boolean isAny(Object value)
    {
        if(value instanceof String)
        {
            if("any".equalsIgnoreCase((String) value)) return true;
            if("all".equalsIgnoreCase((String) value)) return false;
        }
        _logger.warn("Ignoring unrecognised match type: " + value);
        return false;//default to all
    }

    static boolean isSpecial(Object key)
    {
        return key instanceof String && isSpecial((String) key);
    }

    static boolean isSpecial(String key)
    {
        return key.startsWith("X-") || key.startsWith("x-");
    }
}
