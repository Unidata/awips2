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
import java.util.HashMap;
import java.util.Map;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;

public class AmqpMethod implements Printable, NodeAware, VersionConsistencyCheck
{
    private final AmqpVersionSet _versionSet = new AmqpVersionSet();
    private final AmqpFieldMap _fieldMap = new AmqpFieldMap();

    private final AmqpOrdinalVersionMap _indexMap = new AmqpOrdinalVersionMap();
    private final AmqpFlagMap _clientMethodFlagMap = new AmqpFlagMap(); // Method called on client (<chassis name="server"> in XML)
    private final AmqpFlagMap _serverMethodFlagMap = new AmqpFlagMap(); // Method called on server (<chassis name="client"> in XML)

    private final Map<AmqpVersion, AmqpFieldMap> _versionToFieldsMap = new HashMap<AmqpVersion, AmqpFieldMap>();

    private final Map<AmqpVersion, AtomicInteger> _versionToFieldCount = new HashMap<AmqpVersion, AtomicInteger>();

    private final String _name;
    private final Generator _generator;


    public AmqpMethod(String name, Generator generator)
    {
        _name = name;
        _generator = generator;
    }

    public boolean addFromNode(Node methodNode, int ordinal, AmqpVersion version)
            throws AmqpParseException, AmqpTypeMappingException
    {
        _versionSet.add(version);
        boolean serverChassisFlag = false;
        boolean clientChassisFlag = false;
        int index = Utils.getNamedIntegerAttribute(methodNode, "index");
        AmqpVersionSet indexVersionSet = _indexMap.get(index);
        if (indexVersionSet != null)
        {
            indexVersionSet.add(version);
        }
        else
        {
            indexVersionSet = new AmqpVersionSet();
            indexVersionSet.add(version);
            _indexMap.put(index, indexVersionSet);
        }
        NodeList nList = methodNode.getChildNodes();
        AtomicInteger fieldCntr = _versionToFieldCount.get(version);
        if(fieldCntr == null)
        {
            fieldCntr = new AtomicInteger(0);
            _versionToFieldCount.put(version, fieldCntr);
        }
        for (int i = 0; i < nList.getLength(); i++)
        {
            Node child = nList.item(i);
            if (child.getNodeName().compareTo(Utils.ELEMENT_FIELD) == 0)
            {
                String fieldName = _generator.prepareDomainName(Utils.getNamedAttribute(child,
                                                                                        Utils.ATTRIBUTE_NAME));
                AmqpField thisField = _fieldMap.get(fieldName);
                AmqpFieldMap versionSpecificFieldMap = _versionToFieldsMap.get(version);
                if (versionSpecificFieldMap == null)
                {
                    versionSpecificFieldMap = new AmqpFieldMap();
                    _versionToFieldsMap.put(version, versionSpecificFieldMap);
                }


                if (thisField == null)
                {
                    thisField = new AmqpField(fieldName, _generator);
                    _fieldMap.add(fieldName, thisField);
                }

                AmqpField versionSpecificField = new AmqpField(fieldName, _generator);
                versionSpecificFieldMap.add(fieldName, versionSpecificField);

                versionSpecificField.addFromNode(child, fieldCntr.intValue(), version);

                if (!thisField.addFromNode(child, fieldCntr.getAndIncrement(), version))
                {
                    String className = _generator.prepareClassName(Utils.getNamedAttribute(methodNode.getParentNode(),
                                                                                           Utils.ATTRIBUTE_NAME));
                    String methodName = _generator.prepareMethodName(Utils.getNamedAttribute(methodNode,
                                                                                             Utils.ATTRIBUTE_NAME));
                    System.out.println("INFO: Generation supression tag found for field " +
                                       className + "." + methodName + "." + fieldName + " - removing.");
                    thisField.removeVersion(version);
                    _fieldMap.remove(fieldName);
                }
            }
            else if (child.getNodeName().compareTo(Utils.ELEMENT_CHASSIS) == 0)
            {
                String chassisName = Utils.getNamedAttribute(child, Utils.ATTRIBUTE_NAME);
                if (chassisName.compareTo("server") == 0)
                {
                    serverChassisFlag = true;
                }
                else if (chassisName.compareTo("client") == 0)
                {
                    clientChassisFlag = true;
                }
            }
            else if (child.getNodeName().compareTo(Utils.ELEMENT_CODEGEN) == 0)
            {
                String value = Utils.getNamedAttribute(child, Utils.ATTRIBUTE_VALUE);
                if (value.compareTo("no-gen") == 0)
                {
                    return false;
                }
            }
        }
        processChassisFlags(serverChassisFlag, clientChassisFlag, version);
        return true;
    }

    public void removeVersion(AmqpVersion version)
    {
        _clientMethodFlagMap.removeVersion(version);
        _serverMethodFlagMap.removeVersion(version);
        _indexMap.removeVersion(version);
        _fieldMap.removeVersion(version);
        _versionSet.remove(version);
    }

    public void print(PrintStream out, int marginSize, int tabSize)
    {
        String margin = Utils.createSpaces(marginSize);
        String tab = Utils.createSpaces(tabSize);
        out.println(margin + "[M] " + _name + " {" + (_serverMethodFlagMap.isSet() ? "S " +
                                                                                     _serverMethodFlagMap + (
                _clientMethodFlagMap.isSet() ? ", " : "") : "") +
                                                                (_clientMethodFlagMap.isSet()
                                                                 ? "C " + _clientMethodFlagMap : "") + "}" + ": " +
                                                                                                     _versionSet);

        for (Integer thisIndex : _indexMap.keySet())
        {
            AmqpVersionSet indexVersionSet = _indexMap.get(thisIndex);
            out.println(margin + tab + "[I] " + thisIndex + indexVersionSet);
        }

        for (String thisFieldName : _fieldMap.keySet())
        {
            AmqpField thisField = _fieldMap.get(thisFieldName);
            thisField.print(out, marginSize + tabSize, tabSize);
        }
    }

    protected void processChassisFlags(boolean serverFlag, boolean clientFlag, AmqpVersion version)
    {
        AmqpVersionSet versionSet = _serverMethodFlagMap.get(serverFlag);
        if (versionSet != null)
        {
            versionSet.add(version);
        }
        else
        {
            versionSet = new AmqpVersionSet();
            versionSet.add(version);
            _serverMethodFlagMap.put(serverFlag, versionSet);
        }

        versionSet = _clientMethodFlagMap.get(clientFlag);
        if (versionSet != null)
        {
            versionSet.add(version);
        }
        else
        {
            versionSet = new AmqpVersionSet();
            versionSet.add(version);
            _clientMethodFlagMap.put(clientFlag, versionSet);
        }
    }

    public AmqpOverloadedParameterMap getOverloadedParameterLists(AmqpVersionSet globalVersionSet,
                                                                  Generator generator)
            throws AmqpTypeMappingException
    {
        AmqpOverloadedParameterMap parameterVersionMap = new AmqpOverloadedParameterMap();
        for (AmqpVersion thisVersion : globalVersionSet)
        {
            AmqpOrdinalFieldMap ordinalFieldMap = _fieldMap.getMapForVersion(thisVersion, true, generator);
            AmqpVersionSet methodVersionSet = parameterVersionMap.get(ordinalFieldMap);
            if (methodVersionSet == null)
            {
                methodVersionSet = new AmqpVersionSet();
                methodVersionSet.add(thisVersion);
                parameterVersionMap.put(ordinalFieldMap, methodVersionSet);
            }
            else
            {
                methodVersionSet.add(thisVersion);
            }
        }
        return parameterVersionMap;
    }

    public boolean isVersionInterfaceConsistent()
    {
        return isVersionInterfaceConsistent(_generator.getVersionSet());
    }

    public boolean isVersionInterfaceConsistent(AmqpVersionSet globalVersionSet)
    {
         if (!_versionSet.equals(globalVersionSet))
        {
            return false;
        }
        if (!_clientMethodFlagMap.isVersionConsistent(globalVersionSet))
        {
            return false;
        }
        if (!_serverMethodFlagMap.isVersionConsistent(globalVersionSet))
        {
            return false;
        }
        if (!_fieldMap.isVersionInterfaceConsistent(globalVersionSet))
        {
            return false;
        }
        return true;
    }

    public boolean isVersionConsistent()
    {
        return isVersionConsistent(_generator.getVersionSet());
    }


    public boolean isVersionConsistent(AmqpVersionSet globalVersionSet)
    {
        return isVersionInterfaceConsistent(globalVersionSet)
               && _indexMap.isVersionConsistent(globalVersionSet)
               && _fieldMap.isVersionConsistent(globalVersionSet);
    }

    public AmqpVersionSet getVersionSet()
    {
        return _versionSet;
    }

    public AmqpFieldMap getFieldMap()
    {
        return _fieldMap;
    }

    public AmqpOrdinalVersionMap getIndexMap()
    {
        return _indexMap;
    }

    public AmqpFlagMap getClientMethodFlagMap()
    {
        return _clientMethodFlagMap;
    }

    public AmqpFlagMap getServerMethodFlagMap()
    {
        return _serverMethodFlagMap;
    }

    public Map<AmqpVersion, AmqpFieldMap> getVersionToFieldsMap()
    {
        return _versionToFieldsMap;
    }

    public String getName()
    {
        return _name;
    }

    public LanguageConverter getGenerator()
    {
        return _generator;
    }

    public SingleVersionMethod asSingleVersionMethod(AmqpVersion version)
    {
        return new SingleVersionMethod(this, version, _generator);
    }

    public Collection<AmqpField> getFields()
    {
        return _fieldMap.values();
    }

    public boolean isCommon(AmqpField field)
    {
        return field.getVersionSet().equals(getVersionSet()) && field.isTypeAndNameConsistent(_generator);
    }

    public boolean isConsistentServerMethod()
    {
        AmqpVersionSet serverVersions = _serverMethodFlagMap.get(true);
        return (serverVersions != null) && serverVersions.containsAll(_generator.getVersionSet());
    }


    public boolean isConsistentClientMethod()
    {
        AmqpVersionSet clientVersions = _clientMethodFlagMap.get(true);
        return (clientVersions != null) && clientVersions.containsAll(_generator.getVersionSet());
    }

    public boolean isServerMethod(AmqpVersion version)
    {
        AmqpVersionSet serverVersions = _serverMethodFlagMap.get(true);
        return (serverVersions != null) && serverVersions.contains(version);
    }


    public boolean isClientMethod(AmqpVersion version)
    {
        AmqpVersionSet clientVersions = _clientMethodFlagMap.get(true);
        return (clientVersions != null) && clientVersions.contains(version);
    }

    public boolean inAllVersions()
    {
        return _versionSet.containsAll(_generator.getVersionSet());
    }
}
