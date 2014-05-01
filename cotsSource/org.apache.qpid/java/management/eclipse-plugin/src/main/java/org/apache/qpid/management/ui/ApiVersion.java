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
package org.apache.qpid.management.ui;

public class ApiVersion
{
    private int major;
    private int minor;

    public ApiVersion(int major, int minor)
    {
        this.major = major;
        this.minor = minor;
    }

    public int getMajor()
    {
        return major;
    }

    public int getMinor()
    {
        return minor;
    }

    public boolean greaterThanOrEqualTo(int major, int minor)
    {
        if((this.major == major) && (this.minor >= minor))
        {
            return true;
        }
        else if (this.major > major)
        {
            return true;
        }

        return false;
    }

    public boolean lessThanOrEqualTo(int major, int minor)
    {
        if((this.major == major) && (this.minor <= minor))
        {
            return true;
        }
        else if (this.major < major)
        {
            return true;
        }

        return false;
    }

    public boolean greaterThan(int major, int minor)
    {
        if(this.major > major)
        {
            return true;
        }
        else if ((this.major == major) && (this.minor > minor))
        {
            return true;
        }

        return false;
    }

    public boolean lessThan(int major, int minor)
    {
        if(this.major < major)
        {
            return true;
        }
        else if ((this.major == major) && (this.minor < minor))
        {
            return true;
        }

        return false;
    }

    public boolean equals(int major, int minor)
    {
        return (this.major == major) && (this.minor == minor);
    }

    public String toString()
    {
        return new String("major=" + major + ",minor=" + minor);
    }

}
