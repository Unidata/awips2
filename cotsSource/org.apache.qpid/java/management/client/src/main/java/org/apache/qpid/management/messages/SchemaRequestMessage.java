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
package org.apache.qpid.management.messages;

import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.domain.model.type.Binary;

/**
 * Abstract representation of a schema request message.
 * Concrete subclasses must supply the values needed to build & encode the message.
 * 
 * @author Andrea Gazzarini
 */
public abstract class SchemaRequestMessage extends ManagementMessage
{
    @Override
    char opcode ()
    {
        return Protocol.SCHEMA_REQUEST_OPCODE;
    }
    
    /**
     * Returns the package name.
     * 
     * @return the package name.
     */
    protected abstract String packageName();
    
    /**
     * Returns the class name.
     * 
     * @return the class name.
     */
    protected abstract String className();
    
    /**
     * Returns the schema hash.
     * 
     * @return the schema hash.
     */
    protected abstract Binary schemaHash();

    @Override
    final void specificMessageEncoding ()
    {
    	_codec.writeStr8(packageName());
    	_codec.writeStr8(className());
        schemaHash().encode(_codec);
    }
}
