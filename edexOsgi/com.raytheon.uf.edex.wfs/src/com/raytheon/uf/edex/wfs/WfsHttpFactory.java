/*
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
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs;

import org.apache.commons.pool.KeyedPoolableObjectFactory;

import com.raytheon.uf.edex.wfs.provider.OgcWfsProvider;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;

/**
 * TODO Add Description
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsHttpFactory implements KeyedPoolableObjectFactory {

	protected WfsRegistryImpl registry;

	/**
	 * @param registry
	 * @param converter
	 */
	public WfsHttpFactory(WfsRegistryImpl registry) {
		super();
		this.registry = registry;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#makeObject(java.lang
	 * .Object)
	 */
	@Override
	public Object makeObject(Object key) throws Exception {
		return new WfsHttpHandler(new OgcWfsProvider(registry));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#destroyObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void destroyObject(Object key, Object obj) throws Exception {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#validateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public boolean validateObject(Object key, Object obj) {
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#activateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void activateObject(Object key, Object obj) throws Exception {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#passivateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void passivateObject(Object key, Object obj) throws Exception {
		// TODO Auto-generated method stub

	}

}
