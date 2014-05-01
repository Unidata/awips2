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

import java.io.UnsupportedEncodingException;

import java.util.Map;
import java.util.Properties;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Strings
 *
 */

public final class Strings
{

    private static final byte[] EMPTY = new byte[0];

    private static final ThreadLocal<char[]> charbuf = new ThreadLocal()
    {
        public char[] initialValue()
        {
            return new char[4096];
        }
    };

    public static final byte[] toUTF8(String str)
    {
        if (str == null)
        {
            return EMPTY;
        }
        else
        {
            final int size = str.length();
            char[] chars = charbuf.get();
            if (size > chars.length)
            {
                chars = new char[Math.max(size, 2*chars.length)];
                charbuf.set(chars);
            }

            str.getChars(0, size, chars, 0);
            final byte[] bytes = new byte[size];
            for (int i = 0; i < size; i++)
            {
                if (chars[i] > 127)
                {
                    try
                    {
                        return str.getBytes("UTF-8");
                    }
                    catch (UnsupportedEncodingException e)
                    {
                        throw new RuntimeException(e);
                    }
                }

                bytes[i] = (byte) chars[i];
            }
            return bytes;
        }
    }

    public static final String fromUTF8(byte[] bytes)
    {
        try
        {
            return new String(bytes, "UTF-8");
        }
        catch (UnsupportedEncodingException e)
        {
            throw new RuntimeException(e);
        }
    }

    private static final Pattern VAR = Pattern.compile("(?:\\$\\{([^\\}]*)\\})|(?:\\$(\\$))");

    public static interface Resolver
    {
        String resolve(String variable);
    }

    public static class MapResolver implements Resolver
    {

        private final Map<String,String> map;

        public MapResolver(Map<String,String> map)
        {
            this.map = map;
        }

        public String resolve(String variable)
        {
            return map.get(variable);
        }
    }

    public static class PropertiesResolver implements Resolver
    {

        private final Properties properties;

        public PropertiesResolver(Properties properties)
        {
            this.properties = properties;
        }

        public String resolve(String variable)
        {
            return properties.getProperty(variable);
        }
    }

    public static class ChainedResolver implements Resolver
    {
        private final Resolver primary;
        private final Resolver secondary;

        public ChainedResolver(Resolver primary, Resolver secondary)
        {
            this.primary = primary;
            this.secondary = secondary;
        }

        public String resolve(String variable)
        {
            String result = primary.resolve(variable);
            if (result == null)
            {
                result = secondary.resolve(variable);
            }
            return result;
        }
    }

    public static final Resolver SYSTEM_RESOLVER = new Resolver()
    {
        public String resolve(String variable)
        {
            String result = System.getProperty(variable);
            if (result == null)
            {
                result = System.getenv(variable);
            }
            return result;
        }
    };

    public static final String expand(String input)
    {
        return expand(input, SYSTEM_RESOLVER);
    }

    public static final String expand(String input, Resolver resolver)
    {
        return expand(input, resolver, new Stack());
    }

    private static final String expand(String input, Resolver resolver, Stack<String> stack)
    {
        Matcher m = VAR.matcher(input);
        StringBuffer result = new StringBuffer();
        while (m.find())
        {
            String var = m.group(1);
            if (var == null)
            {
                String esc = m.group(2);
                if ("$".equals(esc))
                {
                    m.appendReplacement(result, Matcher.quoteReplacement("$"));
                }
                else
                {
                    throw new IllegalArgumentException(esc);
                }
            }
            else
            {
                m.appendReplacement(result, Matcher.quoteReplacement(resolve(var, resolver, stack)));
            }
        }
        m.appendTail(result);
        return result.toString();
    }

    private static final String resolve(String var, Resolver resolver, Stack<String> stack)
    {
        if (stack.contains(var))
        {
            throw new IllegalArgumentException
                (String.format("recursively defined variable: %s stack=%s", var,
                               stack));
        }

        String result = resolver.resolve(var);
        if (result == null)
        {
            throw new IllegalArgumentException("no such variable: " + var);
        }

        stack.push(var);
        try
        {
            return expand(result, resolver, stack);
        }
        finally
        {
            stack.pop();
        }
    }

}
