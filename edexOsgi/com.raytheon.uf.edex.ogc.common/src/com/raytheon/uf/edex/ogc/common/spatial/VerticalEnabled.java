/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import javax.measure.unit.Unit;

/**
 * Adapter for vertical spatial information
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface VerticalEnabled<T> {

    /**
     * @param obj
     * @return null if object has no vertical information
     */
    public VerticalCoordinate getVerticalCoordinate(T obj);

    /**
     * @return class supported by enabler
     */
    public Class<T> getSupportedClass();

	/**
	 * default unit for vertical coordinates
	 * 
	 * @return null if there is no reliable default
	 */
	public Unit<?> getDefaultVerticalUnit();

}
