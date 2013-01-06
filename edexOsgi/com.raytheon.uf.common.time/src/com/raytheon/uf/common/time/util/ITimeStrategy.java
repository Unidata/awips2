package com.raytheon.uf.common.time.util;

/**
 * 
 * Denotes a strategy to retrieve the current time. In production this will
 * always retrieve it from {@link System#currentTimeMillis()} but in tests it
 * can be used to give the behavior of specific time spans, or to freeze time
 * entirely.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * Nov 02, 2012 1302       djohnson     Add more Javadoc.
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