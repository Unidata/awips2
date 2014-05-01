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
namespace org.apache.qpid.transport.util
{
    /// <summary> 
    /// This class provides basic serial number comparisons as defined in
    /// RFC 1982.
    /// </summary>
    public class Serial
    {
        ///
        /// 
        ///Compares two numbers using serial arithmetic.
        ///
        /// param s1 the first serial number
        /// param s2 the second serial number
        ///
        /// return a negative integer, zero, or a positive integer as the
        /// first argument is less than, equal to, or greater than the
        ///  second
        ///
        public static int Compare(int s1, int s2)
        {
            return s1 - s2;
        }

        public static bool Lt(int s1, int s2)
        {
            return Compare(s1, s2) < 0;
        }

        public static bool Le(int s1, int s2)
        {
            return Compare(s1, s2) <= 0;
        }

        public static bool Gt(int s1, int s2)
        {
            return Compare(s1, s2) > 0;
        }

        public static bool Ge(int s1, int s2)
        {
            return Compare(s1, s2) >= 0;
        }

        public static bool Eq(int s1, int s2)
        {
            return s1 == s2;
        }

        public static int Min(int s1, int s2)
        {
            if (Lt(s1, s2))
            {
                return s1;
            }
            else
            {
                return s2;
            }
        }

        public static int Max(int s1, int s2)
        {
            if (Gt(s1, s2))
            {
                return s1;
            }
            else
            {
                return s2;
            }
        }
    }
}
