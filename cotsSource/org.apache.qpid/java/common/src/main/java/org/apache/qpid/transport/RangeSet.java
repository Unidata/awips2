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
package org.apache.qpid.transport;

import java.util.Iterator;
import java.util.ListIterator;
import java.util.LinkedList;

import static org.apache.qpid.util.Serial.*;

/**
 * RangeSet
 *
 * @author Rafael H. Schloming
 */

public final class RangeSet implements Iterable<Range>
{

    private LinkedList<Range> ranges = new LinkedList<Range>();

    public int size()
    {
        return ranges.size();
    }

    public Iterator<Range> iterator()
    {
        return ranges.iterator();
    }

    public Range getFirst()
    {
        return ranges.getFirst();
    }

    public Range getLast()
    {
        return ranges.getLast();
    }

    public boolean includes(Range range)
    {
        for (Range r : this)
        {
            if (r.includes(range))
            {
                return true;
            }
        }

        return false;
    }

    public boolean includes(int n)
    {
        for (Range r : this)
        {
            if (r.includes(n))
            {
                return true;
            }
        }

        return false;
    }

    public void add(Range range)
    {
        ListIterator<Range> it = ranges.listIterator();

        while (it.hasNext())
        {
            Range next = it.next();
            if (range.touches(next))
            {
                it.remove();
                range = range.span(next);
            }
            else if (lt(range.getUpper(), next.getLower()))
            {
                it.previous();
                it.add(range);
                return;
            }
        }

        it.add(range);
    }

    public void add(int lower, int upper)
    {
        add(new Range(lower, upper));
    }

    public void add(int value)
    {
        add(value, value);
    }

    public void clear()
    {
        ranges.clear();
    }

    public RangeSet copy()
    {
        RangeSet copy = new RangeSet();
        copy.ranges.addAll(ranges);
        return copy;
    }

    public String toString()
    {
        StringBuffer str = new StringBuffer();
        str.append("{");
        boolean first = true;
        for (Range range : ranges)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                str.append(", ");
            }
            str.append(range);
        }
        str.append("}");
        return str.toString();
    }

}
