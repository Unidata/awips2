/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid;

public class QpidException extends Exception
{
    /**
     * AMQP error code
     */
    private ErrorCode _errorCode;

    /**
     * Constructor for a Qpid Exception.
     * <p> This is the only provided constructor and the parameters have to be set to null when
     * they are unknown.
     * @param message    A description of the reason of this exception .
     * @param errorCode  A string specifyin the error code of this exception.
     * @param cause      The linked Execption.    * 
     * 
     */
    public QpidException(String message, ErrorCode errorCode, Throwable cause)
    {
        super(message, cause);
        _errorCode = errorCode;
    }
    
    /*hack to get rid of a compile error from a  generated class
    public QpidException(String message, String errorCode, Throwable cause)
    {
        
    }*/

    /**
     * Get this execption error code.
     *
     * @return This exception error code.
     */
    public ErrorCode getErrorCode()
    {
        return _errorCode;
    }
}

