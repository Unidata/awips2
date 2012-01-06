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

import org.w3c.dom.Attr;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class Utils
{
    public final static String FILE_SEPARATOR = System.getProperty("file.separator");
    public final static String LINE_SEPARATOR = System.getProperty("line.separator");

    public final static String ATTRIBUTE_NAME = "name";
    public final static String ATTRIBUTE_MAJOR = "major";
    public final static String ATTRIBUTE_MINOR = "minor";
    public final static String ATTRIBUTE_INDEX = "index";
    public final static String ATTRIBUTE_LABEL = "label";
    public final static String ATTRIBUTE_SYNCHRONOUS = "synchronous";
    public final static String ATTRIBUTE_CONTENT = "content";
    public final static String ATTRIBUTE_HANDLER = "handler";
    public final static String ATTRIBUTE_DOMAIN = "domain";
    public final static String ATTRIBUTE_VALUE = "value";
    public final static String ATTRIBUTE_TYPE = "type"; // For compatibility with AMQP 8.0

    public final static String ELEMENT_AMQP = "amqp";
    public final static String ELEMENT_CHASSIS = "chassis";
    public final static String ELEMENT_CLASS = "class";
    public final static String ELEMENT_CODEGEN = "codegen";
    public final static String ELEMENT_CONSTANT = "constant";
    public final static String ELEMENT_DOMAIN = "domain";
    public final static String ELEMENT_METHOD = "method";
    public final static String ELEMENT_FIELD = "field";
    public final static String ELEMENT_VERSION = "version";

    // Attribute functions

    public static String getNamedAttribute(Node n, String attrName) throws AmqpParseException
    {
        NamedNodeMap nnm = n.getAttributes();
        if (nnm == null)
        {
            throw new AmqpParseException("Node \"" + n.getNodeName() + "\" has no attributes.");
        }
        Attr a = (Attr) nnm.getNamedItem(attrName);
        if (a == null)
        {
            throw new AmqpParseException("Node \"" + n.getNodeName() + "\" has no attribute \"" + attrName + "\".");
        }
        return a.getNodeValue();
    }

    public static int getNamedIntegerAttribute(Node n, String attrName) throws AmqpParseException
    {
        return Integer.parseInt(getNamedAttribute(n, attrName));
    }

    // Element functions

    public static Node findChild(Node n, String eltName) throws AmqpParseException
    {
        NodeList nl = n.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++)
        {
            Node cn = nl.item(i);
            if (cn.getNodeName().compareTo(eltName) == 0)
            {
                return cn;
            }
        }
        throw new AmqpParseException("Node \"" + n.getNodeName() +
                                     "\" does not contain child element \"" + eltName + "\".");
    }

    // String functions

    public static String firstUpper(String str)
    {
        if (!Character.isLetter(str.charAt(0)) || !Character.isLowerCase(str.charAt(0)))
        {
            return str;
        }
        StringBuffer sb = new StringBuffer(str);
        sb.setCharAt(0, Character.toUpperCase(str.charAt(0)));
        return sb.toString();
    }

    public static String firstLower(String str)
    {
        if (!Character.isUpperCase(str.charAt(0)))
        {
            return str;
        }
        StringBuffer sb = new StringBuffer(str);
        sb.setCharAt(0, Character.toLowerCase(str.charAt(0)));
        return sb.toString();
    }

    public static String createSpaces(int cnt)
    {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < cnt; i++)
        {
            sb.append(' ');
        }
        return sb.toString();
    }

    public static boolean containsOnlyDigits(String str)
    {
        boolean foundNonDigit = false;
        for (int i = 0; i < str.length() && !foundNonDigit; i++)
        {
            if (!Character.isDigit(str.charAt(i)))
            {
                foundNonDigit = true;
            }
        }
        return !foundNonDigit;
    }

    public static boolean containsOnlyDigitsAndDecimal(String str)
    {
        boolean foundNonDigit = false;
        int decimalCntr = 0;
        for (int i = 0; i < str.length() && !foundNonDigit && decimalCntr < 2; i++)
        {
            char ch = str.charAt(i);
            if (!(Character.isDigit(ch) || ch == '.'))
            {
                foundNonDigit = true;
            }
            else if (ch == '.')
            {
                decimalCntr++;
            }
        }
        return !foundNonDigit && decimalCntr < 2;
    }
}
