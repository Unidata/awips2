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
package org.apache.qpid.management.web.action;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.management.ObjectName;

import org.apache.qpid.management.Names;

/**
 * Value Object encapsulating a broker management domain model.
 * 
 * @author Andrea Gazzarini
 */
public class BrokerModel
{
	private Map<String, List<ObjectName>> _objectsByType = new HashMap<String, List<ObjectName>>();
	private String _id;
	
	/**
	 * Adds a new object to this domain model.
	 * 
	 * @param name the object name of the JMX entity.
	 */
	void addObject(ObjectName name)
	{
		String packageName = name.getKeyProperty(Names.PACKAGE);
		String className = name.getKeyProperty(Names.CLASS);
		if (className != null)
		{
			String fqn = packageName+"."+className;
			
			List<ObjectName> objects = _objectsByType.get(fqn);
			if (objects == null)
			{
				objects = new ArrayList<ObjectName>();
				_objectsByType.put(fqn,objects);
			}
			objects.add(name);		
		}
	}

	/**
	 * Gets the identifier of the owner of this model.
	 * 
	 * @return the identifier of the owner of this model.
	 */
	public String getId()
	{
		return _id;
	}

	/**
	 * Sets the identifier of the owner of this model.
	 * 
	 * @param id the identifier of the owner of this model.
	 */
	public void setId(String id)
	{
		this._id = id;
	}
	
	public Set<String> getCategoryNames()
	{
		return _objectsByType.keySet();
	}
	
	public List<ObjectName> getCategory(String name) 
	{
		return _objectsByType.get(name);
	}
	
	public int getCategoryCount()
	{
		return _objectsByType.keySet().size();
	}
}