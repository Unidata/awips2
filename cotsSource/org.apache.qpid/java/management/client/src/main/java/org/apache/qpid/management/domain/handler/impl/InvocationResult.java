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
package org.apache.qpid.management.domain.handler.impl;

import java.io.Serializable;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.qpid.management.domain.services.MethodInvocationException;

/**
 * Value object used for storing an invocation method result.
 * This is done in order to accomplish multiple return value requirement. 
 * As we know, it's not possible to do that only with method signature and therefore this value object / struct is used.
 * 
 * @author Andrea Gazzarini
 */
public class InvocationResult implements Serializable
{
    private static final long serialVersionUID = 2062662997326399693L;
    
    private final long _returnCode;
    private final String _statusText;
    private final byte [] _outputAndBidirectionalArgumentValues;
    private Map<String, Object> _outputSection;
    
    /**
     * Builds an invocation result with the given status code and status text.
     * 
     * @param statusCode the status code.
     * @param statusText the status text.
     */
    InvocationResult(long statusCode, String statusText,byte [] outputAndBidirectionalArgumentValues)
    {
        this._returnCode = statusCode;
        this._statusText = statusText;
        this._outputAndBidirectionalArgumentValues = outputAndBidirectionalArgumentValues;
    }
        
    /**
     * Checks if this result contains an error return code.
     *  
     * @return true if this result object contains an error return code.
     */
    public boolean isException () 
    {
        return _returnCode != 0;
    }
    
    /**
     * Simply throws a new MethodInvocationException.
     * Usually this method is called in conjunction with the isException() method in order to raise an exception if 
     * the wrapped return code means that there was an error.
     * 
     * @throws MethodInvocationException always.
     */
    public void createAndThrowException() throws MethodInvocationException
    {
        throw new MethodInvocationException(_returnCode, _statusText);
    }
    
    @Override
    public String toString ()
    {
        StringBuilder builder = new StringBuilder()
            .append("Status code : ")
            .append(_returnCode)
            .append(",")
            .append("Status Text : ")
            .append(_statusText);
        if (_outputSection != null && !_outputSection.isEmpty())
        {
            builder.append(". Parameters : ");
            for (Entry<String, Object> outputEntry : _outputSection.entrySet())
            {
                builder.append(outputEntry.getKey()).append('=').append(outputEntry.getValue());
                builder.append(',');
            }
        }            
        return builder.toString();
    }

    /**
     * Returns the return code of this invocation result.
     * 
     * @return the return code of this invocation result.
     */
    public long getReturnCode ()
    {
        return _returnCode;
    }

    /**
     * Contains the status text of this invocation result.
     * 
     * @return the status text of this invocation result.
     */
    public String getStatusText ()
    {
        return _statusText;
    }
    
    /**
     * Returns the output and bidirectional argument values in raw format (byte [])
     * 
     * @return the output and bidirectional argument values in raw format (byte [])
     */
    public byte [] getOutputAndBidirectionalArgumentValues()
    {
        return _outputAndBidirectionalArgumentValues;
    }

    /**
     * Sets the output section (decoded) of this invocation result.
     * When an incoming message arrives, the output section (output and bidirectional argument values) are 
     * initially stored in raw format.
     * After that, their values need to be converted. 
     * The final result is a map containing (for each Output or Input/Output parameter) the name of the argument as key 
     * and its value as value. 
     * 
     * @param output a map containing outptu and bidirectional values (not in schema order). 
     */
    public void setOutputSection (Map<String, Object> outputSection)
    {
        this._outputSection = outputSection;
    }
    
    /**
     * Returns the output section of this invocation result. 
     * The output section consists in output and bidirectional argument values.
     * Note that the order of the arguments is not guaranteed.
     * 
     * @param outputSection the output section of this invocation result;
     */
    public Map<String, Object> getOutputSection ()
    {
        return _outputSection;
    }    
}