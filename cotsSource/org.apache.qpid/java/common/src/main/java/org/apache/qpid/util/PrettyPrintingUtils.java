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
package org.apache.qpid.util;

/**
 * Contains pretty printing convenienve methods for producing formatted logging output, mostly for debugging purposes.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * </table>
 *
 * @todo Drop this. There are already array pretty printing methods it java.utils.Arrays.
 */
public class PrettyPrintingUtils
{
    /**
     * Pretty prints an array of ints as a string.
     *
     * @param array The array to pretty print.
     *
     * @return The pretty printed string.
     */
    public static String printArray(int[] array)
    {
        String result = "[";
        for (int i = 0; i < array.length; i++)
        {
            result += array[i];
            result += (i < (array.length - 1)) ? ", " : "";
        }

        result += "]";

        return result;
    }

    /**
     * Pretty prints an array of strings as a string.
     *
     * @param array The array to pretty print.
     *
     * @return The pretty printed string.
     */
    public static String printArray(String[] array)
    {
        String result = "[";
        for (int i = 0; i < array.length; i++)
        {
            result += array[i];
            result += (i < (array.length - 1)) ? ", " : "";
        }

        result += "]";

        return result;
    }
}
