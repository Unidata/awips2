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
package org.apache.qpid.server.management;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.JMException;

import org.apache.qpid.AMQException;

/**
 * This should be implemented by all Managable objects.
 */
public interface ManagedObject
{
    static final String DOMAIN = "org.apache.qpid";

    /**
     * @return the name that uniquely identifies this object instance. It must be
     * unique only among objects of this type at this level in the hierarchy so
     * the uniqueness should not be too difficult to ensure.
     */
    String getObjectInstanceName();

    String getType();

    Class<?> getManagementInterface();

    ManagedObject getParentObject();

    void register() throws AMQException, JMException;

    void unregister() throws AMQException;

    /**
     * Returns the ObjectName required for the mbeanserver registration.
     * @return ObjectName
     * @throws MalformedObjectNameException
     */
    ObjectName getObjectName() throws MalformedObjectNameException;
}
