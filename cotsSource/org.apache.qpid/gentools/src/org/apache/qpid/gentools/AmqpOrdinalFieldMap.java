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

import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;

@SuppressWarnings("serial")
public class AmqpOrdinalFieldMap extends TreeMap<Integer, String[]> implements Comparable
{


    public int compareTo(Object obj)
    {
        AmqpOrdinalFieldMap o = (AmqpOrdinalFieldMap) obj;
        Set<Integer> thisKeySet = keySet();
        Set<Integer> oKeySet = o.keySet();
        if (!thisKeySet.equals(oKeySet)) // Not equal, but why?
        {
            // Size difference
            int sizeDiff = thisKeySet.size() - oKeySet.size(); // -ve if this < other
            if (sizeDiff != 0)
            {
                return sizeDiff;
            }
            // Conetent difference
            Iterator<Integer> itr = thisKeySet.iterator();
            Iterator<Integer> oItr = oKeySet.iterator();
            while (itr.hasNext() && oItr.hasNext())
            {
                int diff = itr.next() - oItr.next(); // -ve if this < other
                if (diff != 0)
                {
                    return diff;
                }
            }
            // We should never get here...
            System.err.println("AmqpOrdinalFieldMap.compareTo(): " +
                               "WARNING - unable to find cause of keySet difference.");
        }
        // Keys are equal, now check the String[]s
        Iterator<Integer> itr = thisKeySet.iterator();
        Iterator<Integer> oItr = oKeySet.iterator();
        while (itr.hasNext() && oItr.hasNext())
        {
            String[] thisPair = get(itr.next());
            String[] oPair = o.get(oItr.next());
            // Size difference
            int sizeDiff = thisPair.length - oPair.length; // -ve if this < other
            if (sizeDiff != 0)
            {
                return sizeDiff;
            }
            // Conetent difference
            for (int i = 0; i < thisPair.length; i++)
            {
                int diff = thisPair[i].compareTo(oPair[i]);
                if (diff != 0)
                {
                    return diff;
                }
            }
        }
        return 0;
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        for (Integer thisOrdinal : keySet())
        {
            String[] pair = get(thisOrdinal);
            sb.append("[" + thisOrdinal + "] " + pair[0] + " : " + pair[1] + Utils.LINE_SEPARATOR);
        }
        return sb.toString();
    }
}
