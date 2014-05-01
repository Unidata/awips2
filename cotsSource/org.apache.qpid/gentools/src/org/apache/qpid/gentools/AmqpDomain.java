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

import java.io.PrintStream;
import java.util.TreeMap;

@SuppressWarnings("serial")
public class AmqpDomain extends TreeMap<String, AmqpVersionSet> implements Printable
{
    private final String _domainName;

    public AmqpDomain(String domainName)
    {
        _domainName = domainName;
    }

    public void addDomain(String domainType, AmqpVersion version) throws AmqpParseException
    {
        AmqpVersionSet versionSet = get(domainType);
        if (versionSet == null) // First time, create new entry
        {
            versionSet = new AmqpVersionSet();
            put(domainType, versionSet);
        }
        versionSet.add(version);
    }

    public String getDomainType(AmqpVersion version)
            throws AmqpTypeMappingException
    {
        for (String thisDomainType : keySet())
        {
            AmqpVersionSet versionSet = get(thisDomainType);
            if (versionSet.contains(version))
            {
                return thisDomainType;
            }
        }
        throw new AmqpTypeMappingException("Unable to find version " + version + ".");
    }

    public boolean hasVersion(String type, AmqpVersion v)
    {
        AmqpVersionSet vs = get(type);
        if (vs == null)
        {
            return false;
        }
        return vs.contains(v);
    }

    public void print(PrintStream out, int marginSize, int tabSize)
    {
        String margin = Utils.createSpaces(marginSize);
        String tab = Utils.createSpaces(tabSize);
        out.println(margin + getDomainName() + ":");

        for (String thisDomainType : keySet())
        {
            AmqpVersionSet vs = get(thisDomainType);
            out.println(margin + tab + thisDomainType + " : " + vs.toString());
        }
    }

    public String getDomainName()
    {
        return _domainName;
    }

}
