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
package org.apache.qpid.configuration;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * PropertyUtils provides helper methods for dealing with Java properties.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Expand system properties into strings with named expansions.
 * </table>
 *
 * @todo Make the lookup method generic by passing in the properties to use for the expansion, rather than hard coding
 *       as system properties. The expansion code has greater potential for re-use that way.
 *
 * @todo Some more property related code could be added to this utils class, which might more appropriately reside under
 *       org.apache.qpid.util. For example standardised code to load properties from a resource name, currently found in
 *       QpidProperties and possibly other places could be moved here.
 */
public class PropertyUtils
{
    /**
     * Given a string that contains substrings of the form <code>${xxx}</code>, looks up the valuea of 'xxx' as a
     * system properties and substitutes tham back into the original string, to provide a property value expanded
     * string.
     *
     * @param value The string to be scanned for property references. May be <code>null</code>, in which case this
     *              method returns immediately with no effect.
     *
     * @return The original string with the properties replaced, or <code>null</code> if the original string is
     *         <code>null</code>.
     *
     * @throws PropertyException If the string contains an opening <code>${</code> without a balancing <code>}</code>,
     *                           or if the property to expand does not exist as a system property.
     */
    public static String replaceProperties(String value) throws PropertyException
    {
        if (value == null)
        {
            return null;
        }

        ArrayList<String> fragments = new ArrayList<String>();
        ArrayList<String> propertyRefs = new ArrayList<String>();
        parsePropertyString(value, fragments, propertyRefs);

        StringBuffer sb = new StringBuffer();
        Iterator j = propertyRefs.iterator();

        for (String fragment : fragments)
        {
            if (fragment == null)
            {
                String propertyName = (String) j.next();

                // try to get it from the project or keys
                // Backward compatibility
                String replacement = System.getProperty(propertyName);

                if (replacement == null)
                {
                    throw new PropertyException("Property ${" + propertyName + "} has not been set", null);
                }

                fragment = replacement;
            }

            sb.append(fragment);
        }

        return sb.toString();
    }

    /**
     * Parses the supplied value for properties which are specified using ${foo} syntax. $X is left as is, and $$
     * specifies a single $.
     *
     * @param value        The property string to parse.
     * @param fragments    Is populated with the string fragments. A null means "insert a property value here. The number
     *                     of nulls in the list when populated is equal to the size of the propertyRefs list.
     * @param propertyRefs Populated with the property names to be added into the final string.
     */
    private static void parsePropertyString(String value, ArrayList<String> fragments, ArrayList<String> propertyRefs)
        throws PropertyException
    {
        int prev = 0;
        int pos;
        // search for the next instance of $ from the 'prev' position
        while ((pos = value.indexOf("$", prev)) >= 0)
        {

            // if there was any text before this, add it as a fragment
            if (pos > 0)
            {
                fragments.add(value.substring(prev, pos));
            }
            // if we are at the end of the string, we tack on a $
            // then move past it
            if (pos == (value.length() - 1))
            {
                fragments.add("$");
                prev = pos + 1;
            }
            else if (value.charAt(pos + 1) != '{')
            {
                // peek ahead to see if the next char is a property or not
                // not a property: insert the char as a literal
                if (value.charAt(pos + 1) == '$')
                {
                    // two $ map to one $
                    fragments.add("$");
                    prev = pos + 2;
                }
                else
                {
                    // $X maps to $X for all values of X!='$'
                    fragments.add(value.substring(pos, pos + 2));
                    prev = pos + 2;
                }
            }
            else
            {
                // property found, extract its name or bail on a typo
                int endName = value.indexOf('}', pos);
                if (endName < 0)
                {
                    throw new PropertyException("Syntax error in property: " + value, null);
                }

                String propertyName = value.substring(pos + 2, endName);
                fragments.add(null);
                propertyRefs.add(propertyName);
                prev = endName + 1;
            }
        }
        // no more $ signs found
        // if there is any tail to the file, append it
        if (prev < value.length())
        {
            fragments.add(value.substring(prev));
        }
    }
}
