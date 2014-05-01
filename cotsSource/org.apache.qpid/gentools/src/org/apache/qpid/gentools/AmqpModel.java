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

public class AmqpModel implements Printable, NodeAware
{
    private final Generator _generator;
    private final AmqpClassMap classMap = new AmqpClassMap();
    private final AmqpVersionSet _versionSet = new AmqpVersionSet();

    private final Map<AmqpVersion, AmqpClassMap> _versionToClassMapMap = new HashMap<AmqpVersion, AmqpClassMap>();

    public AmqpModel(Generator generator)
    {
        _generator = generator;
    }

    public AmqpClassMap getAmqpClassMap(AmqpVersion version)
    {
        return _versionToClassMapMap.get(version);
    }


    public AmqpVersionSet getVersionSet()
    {
        return _versionSet;
    }

    public boolean addFromNode(Node n, int o, AmqpVersion version)
            throws AmqpParseException, AmqpTypeMappingException
    {
        _versionSet.add(version);
        NodeList nList = n.getChildNodes();

        AmqpClassMap versionSpecificClassMap = _versionToClassMapMap.get(version);

        if (versionSpecificClassMap == null)
        {
            versionSpecificClassMap = new AmqpClassMap();
            _versionToClassMapMap.put(version, versionSpecificClassMap);
        }

        int eCntr = 0;
        for (int i = 0; i < nList.getLength(); i++)
        {
            Node c = nList.item(i);
            if (c.getNodeName().compareTo(Utils.ELEMENT_CLASS) == 0)
            {
                String className = _generator.prepareClassName(Utils.getNamedAttribute(c, Utils.ATTRIBUTE_NAME));
                AmqpClass thisClass = classMap.get(className);
                if (thisClass == null)
                {
                    thisClass = new AmqpClass(className, _generator);
                    classMap.put(className, thisClass);
                }

                AmqpClass versionSpecificClass = new AmqpClass(className, _generator);
                versionSpecificClassMap.put(className, versionSpecificClass);

                versionSpecificClass.addFromNode(c, eCntr, version);

                if (!thisClass.addFromNode(c, eCntr++, version))
                {
                    System.out.println("INFO: Generation supression tag found for class " + className + " - removing.");
                    thisClass.removeVersion(version);
                    classMap.remove(className);
                }
            }
        }
        return true;
    }

    public void print(PrintStream out, int marginSize, int tabSize)
    {
        out.println(Utils.createSpaces(marginSize) +
                    "[C]=class; [M]=method; [F]=field; [D]=domain; [I]=index; [O]=ordinal" + Utils.LINE_SEPARATOR);
        out.println(Utils.createSpaces(marginSize) + "Model:");

        for (String thisClassName : classMap.keySet())
        {
            AmqpClass thisClass = classMap.get(thisClassName);
            thisClass.print(out, marginSize + tabSize, tabSize);
        }
    }

    public LanguageConverter getGenerator()
    {
        return _generator;
    }

    public AmqpClassMap getClassMap()
    {
        return classMap;
    }


    public Collection<AmqpClass> getClasses()
    {
        return classMap.values();
    }

    public SingleVersionModel asSingleVersionModel()
    {
        return new SingleVersionModel(this, getVersionSet().first(), _generator);
    }
}
