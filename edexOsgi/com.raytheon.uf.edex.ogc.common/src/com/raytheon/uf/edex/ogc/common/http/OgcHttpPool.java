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
package com.raytheon.uf.edex.ogc.common.http;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.pool.KeyedPoolableObjectFactory;
import org.apache.commons.pool.impl.GenericKeyedObjectPool;

/**
 * @author bclement
 * 
 */
public class OgcHttpPool extends GenericKeyedObjectPool {

	/** The logger */
	private transient Log logger = LogFactory.getLog(getClass());

	public OgcHttpPool(KeyedPoolableObjectFactory ogcFactory) {
		super(ogcFactory);
	}

	@Override
	public Object borrowObject(Object key) {
		Object retVal = null;
		try {
			retVal = super.borrowObject(key);
		} catch (IllegalStateException e) {
			logger.error(
					"Unable to borrow Ogc HTTP instance from pool for key: "
							+ key, e);
			throw new RuntimeException(e);
		} catch (Throwable e) {
			// handle when OSGi removes object but pool still has key
			returnObject(key, retVal);
			clear(key);
			retVal = borrowObject(key);

			if (retVal == null) {
				// it still didn't work, blow up
				logger.error(
						"Unable to borrow Ogc HTTP instance from pool for key: "
								+ key, e);
				throw new RuntimeException(e);
			}
		}
		return retVal;
	}

	@Override
	public void returnObject(Object key, Object borrowed) {
		try {
			if (borrowed != null && key != null) {
				super.returnObject(key, borrowed);
			}
		} catch (Exception e) {
			logger.error("Unable to return Ogc HTTP instance to pool for key: "
					+ key, e);
		}
	}

	public void drain() {
		clear();
	}
}
