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
package org.apache.qpid.console;//

import java.util.ArrayList;

import org.apache.qpid.transport.codec.*;

public class SchemaClass
{
    public static int CLASS_KIND_EVENT = 2;
    public static int CLASS_KIND_TABLE = 1;
    public ArrayList<SchemaArgument> arguments = new ArrayList<SchemaArgument>();
    private ClassKey classKey;
    private int kind;
    private Session session;
    private ClassKey superType;
    public ArrayList<SchemaMethod> methods = new ArrayList<SchemaMethod>();
    public ArrayList<SchemaProperty> properties = new ArrayList<SchemaProperty>();
    public ArrayList<SchemaStatistic> statistics = new ArrayList<SchemaStatistic>();

    public SchemaClass(int kind, ClassKey key, Decoder dec, Session session)
    {
        // System.Console.WriteLine(key.ClassName) ;
        setKind(kind);
        setSession(session);
        this.setKey(key);
        boolean hasSupertype = false; //dec.readUint8() != 0;
        if (kind == CLASS_KIND_TABLE)
        {
            int propCount = dec.readUint16();
            int statCount = dec.readUint16();
            int methodCount = dec.readUint16();
            if (hasSupertype)
            {
                setSuperType(new ClassKey(dec));
            }
            for (int x = 0; x < propCount; x++)
            {
                properties.add(new SchemaProperty(dec));
            }
            for (int x = 0; x < statCount; x++)
            {
                statistics.add(new SchemaStatistic(dec));
            }
            for (int x = 0; x < methodCount; x++)
            {
                methods.add(new SchemaMethod(dec));
            }
        }
        if (kind == CLASS_KIND_EVENT)
        {
            int argCount = dec.readUint16();
            if (hasSupertype)
            {
                setSuperType(new ClassKey(dec));
            }
            for (int x = 0; x < argCount; x++)
            {
                arguments.add(new SchemaArgument(dec, false));
            }
        }
    }

    public ArrayList<SchemaMethod> getAllMethods()
    {
        if (getSuperType() == null)
        {
            return methods;
        } else
        {
            ArrayList<SchemaMethod> allMethods = new ArrayList<SchemaMethod>(
                    methods);
            allMethods.addAll(getSession().getSchema(getSuperType())
                    .getAllMethods());
            return allMethods;
        }
    }

    public ArrayList<SchemaProperty> getAllProperties()
    {
        if (getSuperType() == null)
        {
            return properties;
        } else
        {
            ArrayList<SchemaProperty> allProperties = new ArrayList<SchemaProperty>(
                    properties);
            allProperties.addAll(getSession().getSchema(getSuperType())
                    .getAllProperties());
            return allProperties;
        }
    }

    public ArrayList<SchemaStatistic> getAllStatistics()
    {
        if (getSuperType() == null)
        {
            return statistics;
        } else
        {
            ArrayList<SchemaStatistic> allStats = new ArrayList<SchemaStatistic>(
                    statistics);
            allStats.addAll(getSession().getSchema(getSuperType())
                    .getAllStatistics());
            return allStats;
        }
    }

    public String getClassKeyString()
    {
        return getKey().getKeyString();
    }

    public String getClassName()
    {
        return getKey().getClassName();
    }

    public ClassKey getKey()
    {
        return classKey;
    }

    public int getKind()
    {
        return kind;
    }

    public SchemaMethod getMethod(String name)
    {
        SchemaMethod returnValue = null;
        for (SchemaMethod method : methods)
        {
            if (method.getName().equals(name))
            {
                returnValue = method;
                break;
            }
        }
        return returnValue;
    }

    public String getPackageName()
    {
        return getKey().getPackageName();
    }

    protected Session getSession()
    {
        return session;
    }

    public ClassKey getSuperType()
    {
        return superType;
    }

    public boolean hasSuperType()
    {
        return getSuperType() != null;
    }

    public void setKey(ClassKey value)
    {
        classKey = value;
    }

    public void setKind(int value)
    {
        kind = value;
    }

    protected void setSession(Session value)
    {
        session = value;
    }

    public void setSuperType(ClassKey value)
    {
        superType = value;
    }

    public ArrayList<SchemaProperty> getProperties()
    {
        return properties;
    }

    public void setProperties(ArrayList<SchemaProperty> properties)
    {
        this.properties = properties;
    }

    public ArrayList<SchemaMethod> getMethods()
    {
        return methods;
    }

    public void setMethods(ArrayList<SchemaMethod> methods)
    {
        this.methods = methods;
    }

    public ArrayList<SchemaStatistic> getStatistics()
    {
        return statistics;
    }

    public void setStatistics(ArrayList<SchemaStatistic> statistics)
    {
        this.statistics = statistics;
    }

    public ArrayList<SchemaArgument> getArguments()
    {
        return arguments;
    }

    public void setArguments(ArrayList<SchemaArgument> arguments)
    {
        this.arguments = arguments;
    }

    public ClassKey getClassKey()
    {
        return classKey;
    }

    public void setClassKey(ClassKey classKey)
    {
        this.classKey = classKey;
    }
}
