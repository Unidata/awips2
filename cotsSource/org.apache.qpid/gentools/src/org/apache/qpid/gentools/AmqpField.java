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

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class AmqpField implements Printable, NodeAware, VersionConsistencyCheck
{

    private final AmqpVersionSet _versionSet = new AmqpVersionSet();
    private final AmqpDomainVersionMap _domainMap = new AmqpDomainVersionMap();
    private final AmqpOrdinalVersionMap _ordinalMap = new AmqpOrdinalVersionMap();

    private final String _name;
    private final Generator _generator;

    private final Map<AmqpVersion, String> _versionToDomainMap = new HashMap<AmqpVersion, String>();
    private final Map<AmqpVersion, Integer> _versionToOrdinalMap = new HashMap<AmqpVersion, Integer>();


    public AmqpField(String name, Generator generator)
    {
        _name = name;
        _generator = generator;

    }

    public boolean addFromNode(Node fieldNode, int ordinal, AmqpVersion version)
            throws AmqpParseException, AmqpTypeMappingException
    {
        _versionSet.add(version);
        String domainType;
        // Early versions of the spec (8.0) used the "type" attribute instead of "domain" for some fields.
        try
        {
            domainType = _generator.prepareDomainName(Utils.getNamedAttribute(fieldNode, Utils.ATTRIBUTE_DOMAIN));
        }
        catch (AmqpParseException e)
        {
            domainType = _generator.prepareDomainName(Utils.getNamedAttribute(fieldNode, Utils.ATTRIBUTE_TYPE));
        }
        AmqpVersionSet thisVersionList = _domainMap.get(domainType);
        if (thisVersionList == null) // First time, create new entry
        {
            thisVersionList = new AmqpVersionSet();
            _domainMap.put(domainType, thisVersionList);
        }

        _versionToDomainMap.put(version, domainType);
        _versionToOrdinalMap.put(version, ordinal);

        thisVersionList.add(version);
        thisVersionList = _ordinalMap.get(ordinal);
        if (thisVersionList == null) // First time, create new entry
        {
            thisVersionList = new AmqpVersionSet();
            _ordinalMap.put(ordinal, thisVersionList);
        }
        thisVersionList.add(version);
        NodeList nList = fieldNode.getChildNodes();
        for (int i = 0; i < nList.getLength(); i++)
        {
            Node child = nList.item(i);
            if (child.getNodeName().compareTo(Utils.ELEMENT_CODEGEN) == 0)
            {
                String value = Utils.getNamedAttribute(child, Utils.ATTRIBUTE_VALUE);
                if (value.compareTo("no-gen") == 0)
                {
                    return false;
                }
            }
        }
        return true;
    }

    public void removeVersion(AmqpVersion version)
    {
        _domainMap.removeVersion(version);
        _ordinalMap.removeVersion(version);
        _versionSet.remove(version);
    }

    public boolean isCodeTypeConsistent(LanguageConverter converter)
            throws AmqpTypeMappingException
    {
        if (_domainMap.size() == 1)
        {
            return true; // By definition
        }
        ArrayList<String> codeTypeList = new ArrayList<String>();
        for (String thisDomainName : _domainMap.keySet())
        {
            AmqpVersionSet versionSet = _domainMap.get(thisDomainName);
            String codeType = converter.getGeneratedType(thisDomainName, versionSet.first());
            if (!codeTypeList.contains(codeType))
            {
                codeTypeList.add(codeType);
            }
        }
        return codeTypeList.size() == 1;
    }

    public boolean isConsistent(Generator generator)
            throws AmqpTypeMappingException
    {
        if (!isCodeTypeConsistent(generator))
        {
            return false;
        }
        if (_ordinalMap.size() != 1)
        {
            return false;
        }
        // Since the various doamin names map to the same code type, add the version occurrences
        // across all domains to see we have all possible versions covered
        int vCntr = 0;
        for (String thisDomainName : _domainMap.keySet())
        {
            vCntr += _domainMap.get(thisDomainName).size();
        }
        return vCntr == generator.getVersionSet().size();
    }

    public boolean isTypeAndNameConsistent(Generator generator)
            throws AmqpTypeMappingException
    {
        if (!isCodeTypeConsistent(generator))
        {
            return false;
        }
        // Since the various doamin names map to the same code type, add the version occurrences
        // across all domains to see we have all possible versions covered
        int vCntr = 0;
        for (String thisDomainName : _domainMap.keySet())
        {
            vCntr += _domainMap.get(thisDomainName).size();
        }
        return vCntr == getVersionSet().size();
    }


    public void print(PrintStream out, int marginSize, int tabSize)
    {
        String margin = Utils.createSpaces(marginSize);
        out.println(margin + "[F] " + _name + ": " + _versionSet);

        for (Integer thisOrdinal : _ordinalMap.keySet())
        {
            AmqpVersionSet versionList = _ordinalMap.get(thisOrdinal);
            out.println(margin + "  [O] " + thisOrdinal + " : " + versionList.toString());
        }

        for (String thisDomainName : _domainMap.keySet())
        {
            AmqpVersionSet versionList = _domainMap.get(thisDomainName);
            out.println(margin + "  [D] " + thisDomainName + " : " + versionList.toString());
        }
    }

    public boolean isVersionConsistent(AmqpVersionSet globalVersionSet)
    {
        if (!_versionSet.equals(globalVersionSet))
        {
            return false;
        }
        if (!_domainMap.isVersionConsistent(globalVersionSet))
        {
            return false;
        }
        if (!_ordinalMap.isVersionConsistent(globalVersionSet))
        {
            return false;
        }
        return true;
    }


    public boolean isVersionInterfaceConsistent(AmqpVersionSet globalVersionSet)
    {
        if (!_versionSet.equals(globalVersionSet))
        {
            return false;
        }
        if (!_domainMap.isVersionConsistent(globalVersionSet))
        {
            return false;
        }
        if (!_ordinalMap.isVersionConsistent(globalVersionSet))
        {
            return false;
        }
        return true;
    }

    public String getDomain(AmqpVersion version)
    {
        return _versionToDomainMap.get(version);
    }

    public String getConsistentNativeType()
    {
        return _generator.getNativeType(_generator.getDomainType(getDomain(_versionSet.first()),_versionSet.first()));
    }

    public int getOrdinal(AmqpVersion version)
    {
        return _versionToOrdinalMap.get(version);
    }

    public AmqpVersionSet getVersionSet()
    {
        return _versionSet;
    }

    public AmqpDomainVersionMap getDomainMap()
    {
        return _domainMap;
    }

    public AmqpOrdinalVersionMap getOrdinalMap()
    {
        return _ordinalMap;
    }

    public String getName()
    {
        return _name;
    }

    public LanguageConverter getGenerator()
    {
        return _generator;
    }

    public Map<AmqpVersion, String> getVersionToDomainMap()
    {
        return _versionToDomainMap;
    }

    public Map<AmqpVersion, Integer> getVersionToOrdinalMap()
    {
        return _versionToOrdinalMap;
    }

}
