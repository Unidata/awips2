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
using System.Collections;
using System.Collections.Generic;
using System.Text;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// RangeSet
    /// </summary>
    public sealed class RangeSet : IEnumerable<Range>
    {
        private readonly List<Range> _ranges = new List<Range>();

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public IEnumerator<Range> GetEnumerator()
        {
            return _ranges.GetEnumerator();
        }


        public int Size()
        {
            return _ranges.Count;
        }


        public Range GetFirst()
        {
            return _ranges[0];
        }

        public bool Includes(Range range)
        {
            foreach (Range r in this)
            {
                if (r.Includes(range))
                {
                    return true;
                }
            }

            return false;
        }

        public bool Includes(int n)
        {
            foreach (Range r in this)
            {
                if (r.Includes(n))
                {
                    return true;
                }
            }

            return false;
        }

        public void Add(Range range)
        {
            for (int i = 0; i < _ranges.Count; i++)
            {
                Range r = _ranges[i];
                if (range.Touches(r))
                {
                    _ranges.Remove(r);
                    range = range.Span(r);
                }
                else if (Serial.Lt(range.Upper, r.Lower))
                {
                    _ranges.Insert(i - 1 , range);
                    return;
                }
            }
            _ranges.Add(range);
        }

        public void Add(int lower, int upper)
        {
            Add(new Range(lower, upper));
        }

        public void Add(int value)
        {
            Add(value, value);
        }

        public void Clear()
        {
            _ranges.Clear();
        }

        public RangeSet Copy()
        {
            RangeSet copy = new RangeSet();
            foreach (Range r in _ranges)
            {
                copy._ranges.Add(r);
            }
            return copy;
        }

        public override String ToString()
        {
            StringBuilder str = new StringBuilder();
            str.Append("{");
            bool first = true;
            foreach (Range range in _ranges)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    str.Append(", ");
                }
                str.Append(range);
            }
            str.Append("}");
            return str.ToString();
        }
    }
}
