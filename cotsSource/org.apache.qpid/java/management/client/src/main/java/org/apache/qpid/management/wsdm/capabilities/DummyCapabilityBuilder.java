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
package org.apache.qpid.management.wsdm.capabilities;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanOperationInfo;
import javax.management.ObjectName;

import org.apache.muse.core.Environment;

/**
 * Dummy capability builder used for avoid duplicated builds for the 
 * same class.
 * Basically it acts likes a Null Object when the target capability class has been 
 * already built.
 * 
 * @author Andrea Gazzarini
 */
public class DummyCapabilityBuilder implements IArtifactBuilder
{
	/**
	 * Director callback.
	 * Do nothing here (see class comments above.)
	 */
	public void begin(ObjectName objectName)
	{
	}

	/**
	 * Director callback.
	 * Do nothing here (see class comments above.)
	 */
	public void endAttributes()
	{
	}

	/**
	 * Director callback.
	 * Do nothing here (see class comments above.)
	 */
	public void endOperations()
	{
	}

	/**
	 * Director callback.
	 * Do nothing here (see class comments above.)
	 */
	public void onAttribute(MBeanAttributeInfo attributeMetadata) 
	{
	}

	/**
	 * Director callback.
	 * Do nothing here (see class comments above.)
	 */
	public void onOperation(MBeanOperationInfo operationMetadata)
	{
	}

	/**
	 * Director callback.
	 * Do nothing here (see class comments above.)
	 */
	public void setEnvironment(Environment environment)
	{
	}
}