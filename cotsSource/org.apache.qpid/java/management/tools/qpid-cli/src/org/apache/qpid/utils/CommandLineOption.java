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

package org.apache.qpid.utils;

import java.util.ArrayList;

public class CommandLineOption implements CommandLineOptionConstants
{
    private String type;
    private ArrayList optionValues;

    public CommandLineOption(String type, String[] values)
    {
        setOptionType(type);
        ArrayList arrayList = new ArrayList(values.length);
        for (int i = 0; i < values.length; i++)
        {
            arrayList.add(values[i]);
        }
        this.optionValues = arrayList;
    }

    private void setOptionType(String type)
    {
        // cater for the long options first
        if (type.startsWith("--"))
        {
            type = type.replaceFirst("--", "");
        }
        if (type.startsWith("-"))
        {
            type = type.replaceFirst("-", "");
        }

        // we do not change the case of the option!

        this.type = type;
    }

    /**
     * @param type
     */
    public CommandLineOption(String type, ArrayList values)
    {
        setOptionType(type);

        if (null != values)
        {
            this.optionValues = values;
        }
    }

    /**
     * @return Returns the type.
     * @see CommandLineOptionConstants
     */
    public String getOptionType()
    {
        return type;
    }

    /**
     * @return Returns the optionValues.
     */
    public String getOptionValue()
    {
        if ((optionValues != null) && (optionValues.size() > 0))
        {
            return (String) optionValues.get(0);
        }
        else
        {
            return null;
        }
    }

    /**
     * @return Returns the optionValues.
     */
    public ArrayList getOptionValues()
    {
        return optionValues;
    }

}
