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

import java.net.URISyntaxException;

public class URLSyntaxException extends URISyntaxException
{
    private int _length;

    public URLSyntaxException(String url, String error, int index, int length)
    {
        super(url, error, index);

        _length = length;
    }

    private static String getPositionString(int index, int length)
    {
        StringBuffer sb = new StringBuffer(index + 1);

        for (int i = 0; i < index; i++)
        {
            sb.append(" ");
        }

        if (length > -1)
        {
            for (int i = 0; i < length; i++)
            {
                sb.append('^');
            }
        }

        return sb.toString();
    }


    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append(getReason());

        if (getIndex() > -1)
        {
            if (_length != -1)
            {
                sb.append(" between indicies ");
                sb.append(getIndex());
                sb.append(" and ");
                sb.append(_length);
            }
            else
            {
                sb.append(" at index ");
                sb.append(getIndex());
            }
        }

        sb.append(" ");
        if (getIndex() != -1)
        {
            sb.append("\n");
        }

        sb.append(getInput());

        if (getIndex() != -1)
        {
            sb.append("\n");
            sb.append(getPositionString(getIndex(), _length));
        }

        return sb.toString();
    }


}
