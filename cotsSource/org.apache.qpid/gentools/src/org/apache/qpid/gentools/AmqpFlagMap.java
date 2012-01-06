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
public class AmqpFlagMap extends TreeMap<Boolean, AmqpVersionSet> implements VersionConsistencyCheck
{
    public boolean isSet()
    {
        return containsKey(true);
    }

    public String toString()
    {
        AmqpVersionSet versionSet = get(true);
        if (versionSet != null)
        {
            return versionSet.toString();
        }
        return "";
    }

    public boolean isVersionConsistent(AmqpVersionSet globalVersionSet)
    {
        if (size() != 1)
        {
            return false;
        }
        return get(firstKey()).equals(globalVersionSet);
    }

    public boolean removeVersion(AmqpVersion version)
    {
        Boolean res = false;
        ArrayList<Boolean> removeList = new ArrayList<Boolean>();
        for (Boolean flag : keySet())
        {
            AmqpVersionSet versionSet = get(flag);
            if (versionSet.contains(version))
            {
                versionSet.remove(version);
                if (versionSet.isEmpty())
                {
                    removeList.add(flag);
                }
                res = true;
            }
        }
        // Get rid of flags no longer in use
        for (Boolean flag : removeList)
        {
            remove(flag);
        }
        return res;
    }
}
