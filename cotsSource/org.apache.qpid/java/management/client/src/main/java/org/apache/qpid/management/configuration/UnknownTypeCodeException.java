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
package org.apache.qpid.management.configuration;

/**
 * Thrown when no type is found in configuration associated to the given code.
 * 
 * @author Andrea Gazzarini
 */
public class UnknownTypeCodeException extends Exception
{
    private static final long serialVersionUID = 5440934037645111591L;
    private int _code;

    /**
     * Builds a new UnknownTypeCodeException with the given code.
     * 
     * @param code the access code.
     */
    UnknownTypeCodeException(int code)
    {
        super(String.valueOf(code));
        this._code = code;
    }

    /**
     * Returns the unknown code.
     * 
     * @return the unknown code.
     */
    public int getCode ()
    {
        return _code;
    }
}
