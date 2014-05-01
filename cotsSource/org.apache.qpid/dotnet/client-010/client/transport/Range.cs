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
using System;
using System.Collections.Generic;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport
{
	
	/// <summary> 
	/// Range
	/// </summary>


    public sealed class Range
    {
        private int _lower;
        private int _upper;

        public Range(int lower, int upper)
        {
            _lower = lower;
            _upper = upper;
        }

        public int Lower
        {
            get { return _lower; }
            set { _lower = value; }
        }
        public int Upper
        {
            get { return _upper; }
            set { _upper = value; }
        }

        public bool Includes(int value)
        {
            return Serial.Le(_lower, value) && Serial.Le(value, _upper);
        }

        public bool Includes(Range range)
        {
            return Includes(range._lower) && Includes(range._upper);
        }

        public bool Intersects(Range range)
        {
            return (Includes(range._lower) || Includes(range._upper) ||
                    range.Includes(_lower) || range.Includes(_upper));
        }

        public bool Touches(Range range)
        {
            return (Intersects(range) ||
                    Includes(range._upper + 1) || Includes(range._lower - 1) ||
                    range.Includes(_upper + 1) || range.Includes(_lower - 1));
        }

        public Range Span(Range range)
        {
            return new Range(Serial.Min(_lower, range._lower), Serial.Max(_upper, range._upper));
        }

        public List<Range> Subtract(Range range)
        {
            List<Range> result = new List<Range>();

            if (Includes(range._lower) && Serial.Le(_lower, range._lower - 1))
            {
                result.Add(new Range(_lower, range._lower - 1));
            }

            if (Includes(range._upper) && Serial.Le(range._upper + 1, _upper))
            {
                result.Add(new Range(range._upper + 1, _upper));
            }

            if (result.Count == 0 && !range.Includes(this))
            {
                result.Add(this);
            }

            return result;
        }

        public Range Intersect(Range range)
        {
            int l = Serial.Max(_lower, range._lower);
            int r = Serial.Min(_upper, range._upper);
            return Serial.Gt(l, r) ? null : new Range(l, r);
        }

        public override String ToString()
        {
            return "[" + _lower + ", " + _upper + "]";
        }
    }
}
