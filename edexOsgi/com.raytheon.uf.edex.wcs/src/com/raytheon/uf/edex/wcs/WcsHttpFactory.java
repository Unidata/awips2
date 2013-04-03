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
 * 
 *
 */
package com.raytheon.uf.edex.wcs;

import org.apache.commons.pool.KeyedPoolableObjectFactory;

import com.raytheon.uf.edex.wcs.provider.OgcWcsProvider;

/**
 * @author bclement
 * 
 */
public class WcsHttpFactory implements KeyedPoolableObjectFactory {

	public WcsHttpFactory() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#activateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void activateObject(Object arg0, Object arg1) throws Exception {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#destroyObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void destroyObject(Object arg0, Object arg1) throws Exception {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#makeObject(java.lang
	 * .Object)
	 */
	@Override
	public Object makeObject(Object arg0) throws Exception {
		return new WcsHttpHandler(new OgcWcsProvider());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#passivateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void passivateObject(Object arg0, Object arg1) throws Exception {
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
	public boolean validateObject(Object arg0, Object arg1) {
		// TODO Auto-generated method stub
		return false;
	}

}
