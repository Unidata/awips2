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
package org.apache.qpid.management.wsdm;

import org.mortbay.component.LifeCycle;
import org.mortbay.component.LifeCycle.Listener;

/**
 * Adapter class used to provide an empty (base) implementation of 
 * Lifecycle listener interface.
 * 
 * Adapter test case needs to be informed about the lifecycle of the 
 * deployed QMan application. Only when its deployment is completed the test
 * case can run successfully.
 * 
 * So, following the same logic of Swng event model, this is an adapter that provides
 * empty implementation of the listener interface (see for example MouseAdapter 
 * for mouse events listener) 
 * 
 * @author Andrea Gazzarini
 */
public class WebApplicationLifeCycleListener implements Listener
{
	public void lifeCycleFailure(LifeCycle event, Throwable cause)
	{
	}

	public void lifeCycleStarted(LifeCycle event)
	{
	}

	public void lifeCycleStarting(LifeCycle event)
	{
	}

	public void lifeCycleStopped(LifeCycle event)
	{
	}

	public void lifeCycleStopping(LifeCycle event)
	{
	}
}