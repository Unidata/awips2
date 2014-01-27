/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * base source for OGC
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractOgcSource {

	private final Map<Class<?>, Object> extensionMap = new ConcurrentHashMap<Class<?>, Object>();

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.reg.WfsSource#getExtension(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	public <E> E getExtension(Class<E> c) {
		E rval = (E) extensionMap.get(c);
		if (rval == null) {
			for (Class<?> key : extensionMap.keySet()) {
				if (c.isAssignableFrom(key)) {
					return (E) extensionMap.get(key);
				}
			}
		}
		return rval;
	}

	/**
	 * @param extension
	 */
	public void setExtension(Object extension) {
		extensionMap.put(extension.getClass(), extension);
	}

	/**
	 * @param extensions
	 */
	public void setExtensions(Object[] extensions) {
		for (Object obj : extensions) {
			setExtension(obj);
		}
	}

}
