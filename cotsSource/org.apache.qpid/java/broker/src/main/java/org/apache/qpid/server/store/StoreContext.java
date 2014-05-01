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
package org.apache.qpid.server.store;

import org.apache.log4j.Logger;

/**
 * A context that the store can use to associate with a transactional context. For example, it could store
 * some kind of txn id.
 *
 * @author Apache Software Foundation
 */
public class StoreContext
{
    private static final Logger _logger = Logger.getLogger(StoreContext.class);

    private String _name;
    private Object _payload;


    public StoreContext()
    {
        _name = "StoreContext";
    }

    public StoreContext(String name)
    {
        _name = name;
    }

    public Object getPayload()
    {
        return _payload;
    }

    public void setPayload(Object payload)
    {
        if(_logger.isDebugEnabled())
        {
            _logger.debug("public void setPayload(Object payload = " + payload + "): called");
        }
        _payload = payload;
    }

    /**
     * Prints out the transactional context as a string, mainly for debugging purposes.
     *
     * @return The transactional context as a string.
     */
    public String toString()
    {
        return "<_name = " + _name + ", _payload = " + _payload + ">";
    }

}
