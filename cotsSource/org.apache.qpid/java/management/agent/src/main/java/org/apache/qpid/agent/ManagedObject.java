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

import org.apache.qpid.agent.binding.MethodBinding;
import org.apache.qpid.agent.binding.PropertyBinding;

/**
 * Objects which are to be managed and controlled by the QMF Agent.
 */
public interface ManagedObject
{
    public abstract long getId();

    public abstract Class getObjectClass();

    public abstract Object get(PropertyBinding property);

    public abstract void set(PropertyBinding property, Object value);

    public abstract Object[] invoke(MethodBinding method, Object... args);

    public abstract String getName();

    public abstract void setName(String name);

    public String getManagedClassName();

    public String getManagedPackageName();

    public void setManagedClassName(String aName);

    public void setManagedPackageName(String aName);
}
