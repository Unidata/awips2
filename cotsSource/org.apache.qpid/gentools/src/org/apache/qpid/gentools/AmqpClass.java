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
import java.util.Collection;

public class AmqpClass implements Printable, NodeAware
{

    private final AmqpVersionSet _versionSet = new AmqpVersionSet();
    private final AmqpFieldMap _fieldMap = new AmqpFieldMap();
    private final AmqpMethodMap _methodMap = new AmqpMethodMap();
    private final AmqpOrdinalVersionMap _indexMap = new AmqpOrdinalVersionMap();


    private final String _name;
    private final Generator _generator;

    public AmqpClass(String name, Generator generator)
    {
        _name = name;
        _generator = generator;
    }

    public boolean addFromNode(Node classNode, int ordinal, AmqpVersion version)
            throws AmqpParseException, AmqpTypeMappingException
    {
        getVersionSet().add(version);
        int index = Utils.getNamedIntegerAttribute(classNode, "index");
        AmqpVersionSet indexVersionSet = getIndexMap().get(index);
        if (indexVersionSet != null)
        {
            indexVersionSet.add(version);
        }
        else
        {
            indexVersionSet = new AmqpVersionSet();
            indexVersionSet.add(version);
            getIndexMap().put(index, indexVersionSet);
        }
        NodeList nList = classNode.getChildNodes();
        int fieldCntr = getFieldMap().size();
        for (int i = 0; i < nList.getLength(); i++)
        {
            Node child = nList.item(i);
            if (child.getNodeName().compareTo(Utils.ELEMENT_FIELD) == 0)
            {
                String fieldName = getGenerator().prepareDomainName(Utils.getNamedAttribute(child,
                                                                                            Utils.ATTRIBUTE_NAME));
                AmqpField thisField = getFieldMap().get(fieldName);
                if (thisField == null)
                {
                    thisField = new AmqpField(fieldName, getGenerator());
                    getFieldMap().add(fieldName, thisField);
                }
                if (!thisField.addFromNode(child, fieldCntr++, version))
                {
                    String className = getGenerator().prepareClassName(Utils.getNamedAttribute(classNode,
                                                                                               Utils.ATTRIBUTE_NAME));
                    System.out.println("INFO: Generation supression tag found for field " +
                                       className + "." + fieldName + " - removing.");
                    thisField.removeVersion(version);
                    getFieldMap().remove(fieldName);
                }
            }
            else if (child.getNodeName().compareTo(Utils.ELEMENT_METHOD) == 0)
            {
                String methodName = getGenerator().prepareMethodName(Utils.getNamedAttribute(child,
                                                                                             Utils.ATTRIBUTE_NAME));
                AmqpMethod thisMethod = getMethodMap().get(methodName);
                if (thisMethod == null)
                {
                    thisMethod = new AmqpMethod(methodName, getGenerator());
                    getMethodMap().put(methodName, thisMethod);
                }
                if (!thisMethod.addFromNode(child, 0, version))
                {
                    String className = getGenerator().prepareClassName(Utils.getNamedAttribute(classNode,
                                                                                               Utils.ATTRIBUTE_NAME));
                    System.out.println("INFO: Generation supression tag found for method " +
                                       className + "." + methodName + " - removing.");
                    thisMethod.removeVersion(version);
                    getMethodMap().remove(methodName);
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
        return true;
    }

    public void removeVersion(AmqpVersion version)
    {
        getIndexMap().removeVersion(version);
        getFieldMap().removeVersion(version);
        getMethodMap().removeVersion(version);
        getVersionSet().remove(version);
    }

    public void print(PrintStream out, int marginSize, int tabSize)
    {
        String margin = Utils.createSpaces(marginSize);
        String tab = Utils.createSpaces(tabSize);
        out.println(margin + "[C] " + getName() + ": " + getVersionSet());

        for (Integer thisIndex : getIndexMap().keySet())
        {
            AmqpVersionSet indexVersionSet = getIndexMap().get(thisIndex);
            out.println(margin + tab + "[I] " + thisIndex + indexVersionSet);
        }

        for (String thisFieldName : getFieldMap().keySet())
        {
            AmqpField thisField = getFieldMap().get(thisFieldName);
            thisField.print(out, marginSize + tabSize, tabSize);
        }

        for (String thisMethodName : getMethodMap().keySet())
        {
            AmqpMethod thisMethod = getMethodMap().get(thisMethodName);
            thisMethod.print(out, marginSize + tabSize, tabSize);
        }
    }

    public AmqpVersionSet getVersionSet()
    {
        return _versionSet;
    }

    public Generator getGenerator()
    {
        return _generator;
    }


    public AmqpFieldMap getFieldMap()
    {
        return _fieldMap;
    }


    public AmqpMethodMap getMethodMap()
    {
        return _methodMap;
    }

    public Collection<AmqpMethod> getMethods()
    {
        return getMethodMap().values();
    }


    public String getName()
    {
        return _name;
    }


    public AmqpOrdinalVersionMap getIndexMap()
    {
        return _indexMap;
    }

    public SingleVersionClass asSingleVersionClass(AmqpVersion version)
    {
        return new SingleVersionClass(this,version, _generator);
    }

}
