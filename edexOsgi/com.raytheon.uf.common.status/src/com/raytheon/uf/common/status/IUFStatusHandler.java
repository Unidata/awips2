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
package com.raytheon.uf.common.status;

import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Describes a method of handling UFStatuses
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008  1433       chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IUFStatusHandler {
    /**
     * 
     * @param p
     * @return
     */
    public boolean isPriorityEnabled(Priority p);

    /**
     * Send a message to Status handler for logging/display.
     * 
     * @param status
     */
    public void handle(UFStatus status);

	/**
	 * Send a message to Status handler for logging/display.
	 * 
	 * @param status
	 */
	public void handle(UFStatus status, String category);

	/**
	 * Send a message to Status handler for logging/display.
	 * 
	 * @param priority
	 *            Message priority.
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
    public void handle(Priority priority, String message);

	/**
	 * Send a message to Status handler for logging/display.
	 * 
	 * @param priority
	 *            Message priority.
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
	public void handle(Priority priority, String category, String message);

	/**
	 * Send a message to Status handler for logging/display.
	 * 
	 * @param priority
	 *            Message priority.
	 * @param message
	 *            Text to be displayed in the message
	 * @param throwable
	 *            associated exception
	 * 
	 * @see UFStatus
	 */
    public void handle(Priority priority, String message, Throwable throwable);

	/**
	 * Send a message to Status handler for logging/display.
	 * 
	 * @param priority
	 *            Message priority.
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * @param throwable
	 *            associated exception
	 * 
	 * @see UFStatus
	 */
	public void handle(Priority priority, String category, String message,
			Throwable throwable);

	/**
	 * Send a debug message to Status handler for logging/display.
	 * 
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
    public void debug(String message);

	/**
	 * Send a debug message to Status handler for logging/display.
	 * 
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
	public void debug(String category, String message);

    /**
     * Send an info message to Status handler for logging/display.
     * 
     * @param message
     *            Text to be displayed in the message
     * 
     * @see UFStatus
     */
    public void info(String message);

	/**
	 * Send an info message to Status handler for logging/display.
	 * 
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
	public void info(String category, String message);

	/**
	 * Send a warn message to Status handler for logging/display.
	 * 
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
    public void warn(String message);

	/**
	 * Send a warn message to Status handler for logging/display.
	 * 
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
	public void warn(String category, String message);

	/**
	 * Send an error message to Status handler for logging/display.
	 * 
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
    public void error(String message);

	/**
	 * Send an error message to Status handler for logging/display.
	 * 
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * 
	 * @see UFStatus
	 */
    public void error(String category, String message);

	/**
	 * Send an error message to Status handler for logging/display.
	 * 
	 * @param message
	 *            Text to be displayed in the message
	 * @param throwable
	 *            associated exception
	 * 
	 * @see UFStatus
	 */
    public void error(String message, Throwable throwable);

	/**
	 * Send an error message to Status handler for logging/display.
	 * 
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * @param throwable
	 *            associated exception
	 * 
	 * @see UFStatus
	 */
	public void error(String category, String message, Throwable throwable);

	/**
	 * Send a fatal message to Status handler for logging/display.
	 * 
	 * @param message
	 *            Text to be displayed in the message
	 * @param throwable
	 *            associated exception
	 * 
	 * @see UFStatus
	 */
    public void fatal(String message, Throwable throwable);

	/**
	 * Send a fatal message to Status handler for logging/display.
	 * 
	 * @param category
	 *            Message category
	 * @param message
	 *            Text to be displayed in the message
	 * @param throwable
	 *            associated exception
	 * 
	 * @see UFStatus
	 */
	public void fatal(String category, String message, Throwable throwable);
}
