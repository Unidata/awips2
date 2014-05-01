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

import java.util.ArrayList;
import java.util.List;

import static org.apache.qpid.util.Serial.*;


/**
 * Range
 *
 * @author Rafael H. Schloming
 */

public final class Range
{
    private final int lower;
    private final int upper;

    public Range(int lower, int upper)
    {
        this.lower = lower;
        this.upper = upper;
    }

    public int getLower()
    {
        return lower;
    }

    public int getUpper()
    {
        return upper;
    }

    public boolean includes(int value)
    {
        return le(lower, value) && le(value, upper);
    }

    public boolean includes(Range range)
    {
        return includes(range.lower) && includes(range.upper);
    }

    public boolean intersects(Range range)
    {
        return (includes(range.lower) || includes(range.upper) ||
                range.includes(lower) || range.includes(upper));
    }

    public boolean touches(Range range)
    {
        return (intersects(range) ||
                includes(range.upper + 1) || includes(range.lower - 1) ||
                range.includes(upper + 1) || range.includes(lower - 1));
    }

    public Range span(Range range)
    {
        return new Range(min(lower, range.lower), max(upper, range.upper));
    }

    public List<Range> subtract(Range range)
    {
        List<Range> result = new ArrayList<Range>();

        if (includes(range.lower) && le(lower, range.lower - 1))
        {
            result.add(new Range(lower, range.lower - 1));
        }

        if (includes(range.upper) && le(range.upper + 1, upper))
        {
            result.add(new Range(range.upper + 1, upper));
        }

        if (result.isEmpty() && !range.includes(this))
        {
            result.add(this);
        }

        return result;
    }

    public Range intersect(Range range)
    {
        int l = max(lower, range.lower);
        int r = min(upper, range.upper);
        if (gt(l, r))
        {
            return null;
        }
        else
        {
            return new Range(l, r);
        }
    }

    public String toString()
    {
        return "[" + lower + ", " + upper + "]";
    }

}
