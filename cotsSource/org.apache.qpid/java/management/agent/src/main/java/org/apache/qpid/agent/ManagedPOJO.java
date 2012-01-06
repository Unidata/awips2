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
package org.apache.qpid.agent;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.agent.binding.BindingUtils;
import org.apache.qpid.agent.binding.MethodBinding;
import org.apache.qpid.agent.binding.PropertyBinding;

/**
 * Wrapper classe for adding POJOS which are to be managed by the QMF Agent.
 */
public class ManagedPOJO extends ManagedObjectBase implements ManagedObject
{
    private Log log = LogFactory.getLog(ManagedPOJO.class);
    private Object managed;

    public ManagedPOJO()
    {
        super();
    }

    public ManagedPOJO(Object managed)
    {
        super();
        this.setManaged(managed);
    }

    @Override
    public long getId()
    {
        if (managed == null)
        {
            throw new AgentException("The managed object is null");
        }
        return System.identityHashCode(managed);
    }

    public Class getObjectClass()
    {
        return managed.getClass();
    }

    public Object getManaged()
    {
        return managed;
    }

    public void setManaged(Object managed)
    {
        this.managed = managed;
    }

    @Override
    public Object get(PropertyBinding property)
    {
        return BindingUtils.get(property, managed);
    }

    @Override
    public Object[] invoke(MethodBinding method, Object... args)
    {
        return BindingUtils.invoke(method, managed, args);
    }

    @Override
    public void set(PropertyBinding property, Object value)
    {
        BindingUtils.set(property, value, managed);
    }
}
