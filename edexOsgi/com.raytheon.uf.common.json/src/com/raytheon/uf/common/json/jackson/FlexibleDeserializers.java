/**********************************************************************
*
* The following software products were developed by Raytheon:
*
* ADE (AWIPS Development Environment) software
* CAVE (Common AWIPS Visualization Environment) software
* EDEX (Environmental Data Exchange) software
* uFrame™ (Universal Framework) software
*
* Copyright (c) 2010 Raytheon Co.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/org/documents/epl-v10.php
*
*
* Contractor Name: Raytheon Company
* Contractor Address:
* 6825 Pine Street, Suite 340
* Mail Stop B8
* Omaha, NE 68106
* 402.291.0100
*
**********************************************************************/
/**
 * 
 */
package com.raytheon.uf.common.json.jackson;

import java.util.HashMap;

import org.codehaus.jackson.map.JsonDeserializer;
import org.codehaus.jackson.map.module.SimpleDeserializers;
import org.codehaus.jackson.map.type.ClassKey;

/**
 * @author bclement
 * 
 */
public class FlexibleDeserializers extends SimpleDeserializers {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.codehaus.jackson.map.module.SimpleDeserializers#addDeserializer(java
	 * .lang.Class, org.codehaus.jackson.map.JsonDeserializer)
	 */
	public <T> void addFlexDeserializer(Class<?> forClass,
			JsonDeserializer<?> deser) {
		ClassKey key = new ClassKey(forClass);
		if (_classMappings == null) {
			_classMappings = new HashMap<ClassKey, JsonDeserializer<?>>();
		}
		_classMappings.put(key, deser);
	}
}
