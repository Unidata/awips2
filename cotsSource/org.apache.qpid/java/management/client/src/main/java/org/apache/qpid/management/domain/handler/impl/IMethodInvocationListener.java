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

import java.util.EventListener;

import org.apache.qpid.management.domain.model.InvocationEvent;

/**
 * Listener interface used to denote a component interested in method invocation events.
 * 
 * @author Andrea Gazzarini
 */
public interface IMethodInvocationListener extends EventListener
{
    /**
     * An operation is going to be invoked on a specific object instance.
     * This lets this listener to be informed about the imminent invocation.
     * 
     * @param event the invocation event.
     */
    void operationIsGoingToBeInvoked(InvocationEvent event);
}