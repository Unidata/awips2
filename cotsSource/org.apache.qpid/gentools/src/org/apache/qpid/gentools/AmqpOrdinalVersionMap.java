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

import java.util.ArrayList;
import java.util.TreeMap;

@SuppressWarnings("serial")
public class AmqpOrdinalVersionMap extends TreeMap<Integer, AmqpVersionSet> implements VersionConsistencyCheck
{
    public boolean isVersionConsistent(AmqpVersionSet globalVersionSet)
    {
        if (size() != 1)
        {
            return false;
        }
        return get(firstKey()).equals(globalVersionSet);
    }

    public int getOrdinal(AmqpVersion version)
            throws AmqpTypeMappingException
    {
        for (Integer thisOrdinal : keySet())
        {
            AmqpVersionSet versionSet = get(thisOrdinal);
            if (versionSet.contains(version))
            {
                return thisOrdinal;
            }
        }
        throw new AmqpTypeMappingException("Unable to locate version " + version + " in ordianl version map.");
    }

    public boolean removeVersion(AmqpVersion version)
    {
        Boolean res = false;
        ArrayList<Integer> removeList = new ArrayList<Integer>();
        for (Integer ordinal : keySet())
        {
            AmqpVersionSet versionSet = get(ordinal);
            if (versionSet.contains(version))
            {
                versionSet.remove(version);
                if (versionSet.isEmpty())
                {
                    removeList.add(ordinal);
                }
                res = true;
            }
        }
        // Get rid of ordinals no longer in use
        for (Integer ordinal : removeList)
        {
            remove(ordinal);
        }
        return res;
    }
}
