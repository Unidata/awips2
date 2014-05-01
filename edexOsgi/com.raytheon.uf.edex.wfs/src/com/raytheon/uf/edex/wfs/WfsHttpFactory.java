/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.wfs;

import org.apache.commons.pool.KeyedPoolableObjectFactory;

import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.v1_1_0.Wfs1_1_0Provider;

/**
 * Http handler factory for WFS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2011            bclement     Initial creation
 * 
 * </pre>
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
		return new WfsHttpHandler(new Wfs1_1_0Provider(registry));
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
