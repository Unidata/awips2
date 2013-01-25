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

import java.util.List;

import org.codehaus.jackson.Version;
import org.codehaus.jackson.map.JsonDeserializer;
import org.codehaus.jackson.map.module.SimpleModule;

/**
 * @author bclement
 * 
 */
public class FlexibleModule extends SimpleModule {

	/**
	 * @param name
	 * @param version
	 */
	public FlexibleModule(String name, Version version) {
		super(name, version);
		this._deserializers = new FlexibleDeserializers();
	}

	public <T> SimpleModule addDeserializers(List<Class<? extends T>> types,
			JsonDeserializer<T> deser) {
		for (Class<?> c : types) {
			((FlexibleDeserializers) this._deserializers).addFlexDeserializer(
					c, deser);
		}
		return this;
	}
}
