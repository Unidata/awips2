package com.raytheon.uf.common.time.util;

/**
 * 
 * Denotes a strategy to retrieve the current time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
interface ITimeStrategy {

	/**
	 * Retrieves the current time in milliseconds.
	 * 
	 * @return the current time in milliseconds
	 */
	long currentTimeMillis();
}