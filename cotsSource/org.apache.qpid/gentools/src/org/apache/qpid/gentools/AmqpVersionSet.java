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

import java.io.PrintStream;
import java.util.Iterator;
import java.util.TreeSet;

@SuppressWarnings("serial")
public class AmqpVersionSet extends TreeSet<AmqpVersion> implements Printable, Comparable<AmqpVersionSet>
{
    public AmqpVersionSet()
    {
        super();
    }

    public AmqpVersionSet(AmqpVersion version)
    {
        super();
        add(version);
    }

    public AmqpVersion find(AmqpVersion version)
    {
        for (AmqpVersion v : this)
        {
            if (v.compareTo(version) == 0)
            {
                return v;
            }
        }
        return null;
    }

    public void print(PrintStream out, int marginSize, int tabSize)
    {
        out.print(Utils.createSpaces(marginSize) + "Version Set: " + toString() + Utils.LINE_SEPARATOR);
    }

    public int compareTo(AmqpVersionSet other)
    {
        int res = size() - other.size();
        if (res != 0)
        {
            return res;
        }
        Iterator<AmqpVersion> vItr = iterator();
        Iterator<AmqpVersion> oItr = other.iterator();
        while (vItr.hasNext() && oItr.hasNext())
        {
            AmqpVersion version = vItr.next();
            AmqpVersion oVersion = oItr.next();
            res = version.compareTo(oVersion);
            if (res != 0)
            {
                return res;
            }
        }
        return 0;
    }
}
