package org.apache.qpid.console;
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


public class XMLUtil
{
    public static String commonAttributes(SchemaVariable var)
    {
        String returnString = "";
        if (var.getDescription() != null)
        {
            returnString = returnString
                    + String.format(" desc='%s'", var.getDescription());
        }
        if (var.getRefPackage() != null)
        {
            returnString = returnString
                    + String.format(" refPackage='%s'", var.getRefPackage());
        }
        if (var.getRefClass() != null)
        {
            returnString = returnString
                    + String.format(" refClass='%s'", var.getRefClass());
        }
        if (var.getUnit() != null)
        {
            returnString = returnString
                    + String.format(" unit='%s'", var.getUnit());
        }
        if (var.getMin() != null)
        {
            returnString = returnString
                    + String.format(" min='%s'", var.getMin());
        }
        if (var.getMax() != null)
        {
            returnString = returnString
                    + String.format(" max='%s'", var.getMax());
        }
        if (var.getMaxLength() != null)
        {
            returnString = returnString
                    + String.format(" maxLength='%s'", var.getMaxLength());
        }
        return returnString;
    }

    public static String schemaXML(Session sess, String packageName)
    {
        String returnValue = String.format("<schema package='%s'>\n",
                packageName);
        for (ClassKey key : sess.getClasses(packageName))
        {
            SchemaClass schema = sess.getSchema(key);
            if (schema.getKind() == 1)
            {
                if (schema.getSuperType() == null)
                {
                    returnValue += String.format(
                            "\t<class name='%s' hash='%s'>\n", key
                                    .getClassName(), key.getHashString());
                } else
                {
                    returnValue += String.format(
                            "\t<class name='%s' hash='%s' extends='%s'>\n", key
                                    .getClassName(), key.getHashString(),
                            schema.getSuperType().getKeyString());
                }
                for (SchemaProperty prop : schema.getProperties())
                {
                    Object[] attributes = new Object[5];
                    attributes[0] = prop.getName();
                    attributes[1] = Util.typeName(prop.getType());
                    attributes[2] = Util.accessName(prop.getAccess());
                    attributes[3] = prop.getOptional() ? "True" : "False ";
                    attributes[4] = XMLUtil.commonAttributes(prop);
                    returnValue += String
                            .format(
                                    "\t\t<property name='%s' type='%s' access='%s' optional='%s'%s/>\n",
                                    attributes);
                }
                for (SchemaMethod meth : schema.getMethods())
                {
                    returnValue += String.format("\t\t<method name='%s'/>\n",
                            meth.getName());
                    for (SchemaArgument arg : meth.Arguments)
                    {
                        Object[] attributes = new Object[4];
                        attributes[0] = arg.getName();
                        attributes[1] = arg.getDirection();
                        attributes[2] = Util.typeName(arg.getType());
                        attributes[3] = XMLUtil.commonAttributes(arg);
                        returnValue += String
                                .format(
                                        "\t\t\t<arg name='%s' dir='%s' type='%s'%s/>\n",
                                        attributes);
                    }
                    returnValue += String.format("\t\t</method>\n");
                }
                returnValue += String.format("\t</class>\n");
            } else
            {
                returnValue += String.format("\t<event name='%s' hash='%s'>\n",
                        key.getClassName(), key.getHashString());
                for (SchemaArgument arg : schema.getArguments())
                {
                    Object[] attributes = new Object[4];
                    attributes[0] = arg.getName();
                    attributes[1] = Util.typeName(arg.getType());
                    attributes[2] = XMLUtil.commonAttributes(arg);
                    returnValue += String.format(
                            "\t\t\t<arg name='%s' type='%s'%s/>\n", attributes);
                }
                returnValue += String.format("\t</event>\n");
            }
        }
        returnValue += String.format("</schema>\n");
        return returnValue;
    }

    public static String schemaXML(Session sess, String[] packageNames)
    {
        String returnValue = "<schemas>\n";
        for (String pack : packageNames)
        {
            returnValue += XMLUtil.schemaXML(sess, pack);
            returnValue += "\n";
        }
        returnValue += "</schemas>\n";
        return returnValue;
    }

    protected XMLUtil()
    {
    }
}
