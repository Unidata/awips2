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
package org.apache.qpid.url;

import java.util.HashMap;
import java.util.Map;

public class URLHelper
{
    public static char DEFAULT_OPTION_SEPERATOR = '&';
    public static char ALTERNATIVE_OPTION_SEPARATOR = ',';
    public static char BROKER_SEPARATOR = ';';

    public static void parseOptions(Map<String, String> optionMap, String options) throws URLSyntaxException
    {
        // options looks like this
        // brokerlist='tcp://host:port?option='value',option='value';vm://:3/virtualpath?option='value'',failover='method?option='value',option='value''

        if ((options == null) || (options.indexOf('=') == -1))
        {
            return;
        }

        int optionIndex = options.indexOf('=');

        String option = options.substring(0, optionIndex);

        int length = options.length();

        int nestedQuotes = 0;

        // to store index of final "'"
        int valueIndex = optionIndex;

        // Walk remainder of url.
        while ((nestedQuotes > 0) || (valueIndex < length))
        {
            valueIndex++;

            if (valueIndex >= length)
            {
                break;
            }

            if (options.charAt(valueIndex) == '\'')
            {
                if ((valueIndex + 1) < options.length())
                {
                    if ((options.charAt(valueIndex + 1) == DEFAULT_OPTION_SEPERATOR)
                            || (options.charAt(valueIndex + 1) == ALTERNATIVE_OPTION_SEPARATOR)
                            || (options.charAt(valueIndex + 1) == BROKER_SEPARATOR)
                            || (options.charAt(valueIndex + 1) == '\''))
                    {
                        nestedQuotes--;

                        if (nestedQuotes == 0)
                        {
                            // We've found the value of an option
                            break;
                        }
                    }
                    else
                    {
                        nestedQuotes++;
                    }
                }
                else
                {
                    // We are at the end of the string
                    // Check to see if we are corectly closing quotes
                    if (options.charAt(valueIndex) == '\'')
                    {
                        nestedQuotes--;
                    }

                    break;
                }
            }
        }

        if ((nestedQuotes != 0) || (valueIndex < (optionIndex + 2)))
        {
            int sepIndex = 0;

            // Try and identify illegal separator character
            if (nestedQuotes > 1)
            {
                for (int i = 0; i < nestedQuotes; i++)
                {
                    sepIndex = options.indexOf('\'', sepIndex);
                    sepIndex++;
                }
            }

            if ((sepIndex >= options.length()) || (sepIndex == 0))
            {
                throw parseError(valueIndex, "Unterminated option", options);
            }
            else
            {
                throw parseError(sepIndex, "Unterminated option. Possible illegal option separator:'"
                    + options.charAt(sepIndex) + "'", options);
            }
        }

        // optionIndex +2 to skip "='"
        String value = options.substring(optionIndex + 2, valueIndex);

        optionMap.put(option, value);

        if (valueIndex < (options.length() - 1))
        {
            // Recurse to get remaining options
            parseOptions(optionMap, options.substring(valueIndex + 2));
        }
    }

    public static URLSyntaxException parseError(int index, String error, String url)
    {
        return parseError(index, 1, error, url);
    }

    public static URLSyntaxException parseError(int index, int length, String error, String url)
    {
        return new URLSyntaxException(url, error, index, length);
    }

    public static String printOptions(Map<String, String> options)
    {
        if (options.isEmpty())
        {
            return "";
        }
        else
        {
            StringBuffer sb = new StringBuffer();
            sb.append('?');
            for (String key : options.keySet())
            {
                sb.append(key);

                sb.append("='");

                sb.append(options.get(key));

                sb.append("'");
                sb.append(DEFAULT_OPTION_SEPERATOR);
            }

            sb.deleteCharAt(sb.length() - 1);

            return sb.toString();
        }
    }
}
