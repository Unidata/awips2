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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.pool.impl.GenericKeyedObjectPool;

/**
 * @author bclement
 * 
 */
public class JacksonPool extends GenericKeyedObjectPool {

	private ConcurrentMap<Object, ClassLoader> classLoaderPool = new ConcurrentHashMap<Object, ClassLoader>();

	protected boolean poolClassloaders = false;

	public JacksonPool(JacksonFactory jFactory) {
		super(jFactory);
	}

	public JacksonPool() {
		super(new JacksonFactory());
	}

	public JacksonPool(boolean poolClassloaders) {
		super(new JacksonFactory());
		this.poolClassloaders = poolClassloaders;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.impl.GenericKeyedObjectPool#borrowObject(java
	 * .lang.Object)
	 */
	@Override
	public Object borrowObject(Object arg0) throws Exception {
		if (poolClassloaders) {
			classLoaderPool.put(arg0, Thread.currentThread()
					.getContextClassLoader());
		}
		return super.borrowObject(arg0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.impl.GenericKeyedObjectPool#returnObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void returnObject(Object arg0, Object arg1) throws Exception {
		if (poolClassloaders) {
			ClassLoader classLoader = classLoaderPool.get(arg0);
			if (classLoader == null) {
				throw new Exception("Unable to find previous class loader for "
						+ arg0);
			}
			Thread.currentThread().setContextClassLoader(classLoader);
		}
		super.returnObject(arg0, arg1);
	}


}
