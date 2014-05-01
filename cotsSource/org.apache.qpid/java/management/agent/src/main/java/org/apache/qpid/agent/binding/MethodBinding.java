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
package org.apache.qpid.agent.binding;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.transport.codec.Encoder;

/**
 * Metadata for mapping a method to a QMF schema
 */
public class MethodBinding
{
    private final String name;
    private final List<ParameterBinding> parameters;
    private final List<ParameterBinding> inParameters = new ArrayList<ParameterBinding>();
    private final List<ParameterBinding> outParameters = new ArrayList<ParameterBinding>();
    private Log log = LogFactory.getLog(MethodBinding.class);

    public MethodBinding(String name, List<ParameterBinding> parameters)
    {
        this.name = name;
        this.parameters = parameters;
        for (ParameterBinding p : parameters)
        {
            if (p.isIn())
            {
                inParameters.add(p);
            }
            if (p.isOut())
            {
                outParameters.add(p);
            }
        }
    }

    public String getName()
    {
        return name;
    }

    public List<ParameterBinding> getParameters()
    {
        return parameters;
    }

    public List<ParameterBinding> getInParameters()
    {
        return inParameters;
    }

    public List<ParameterBinding> getOutParameters()
    {
        return outParameters;
    }

    void encode(Encoder enc)
    {
        Map map = new HashMap();
        map.put("name", name);
        map.put("argCount", parameters.size());
        enc.writeMap(map);
        for (ParameterBinding p : parameters)
        {
            p.encode(enc);
        }
    }
}
