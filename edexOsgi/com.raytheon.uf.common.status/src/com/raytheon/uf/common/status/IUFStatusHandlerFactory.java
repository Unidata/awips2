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
 * Describes a method of creating UFStatusHandlerFactory
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2010 3265       rjpeter     Initial creation
 * Oct 23, 2013 2303       bgonzale    Added method for logging.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public interface IUFStatusHandlerFactory {
    /**
     * Gets an instance of the handler.
     */
    public IUFStatusHandler getInstance();

    /**
     * Returns the named handler if it is found; null if it is not.
     * 
     * @param name
     * @return handler
     */
    public IUFStatusHandler hasHandler(String name);

    /**
     * Gets a named instance of the handler.
     */
    public IUFStatusHandler getInstance(String name);

    /**
     * Get a handler instance configured for a specific class.
     */
    public IUFStatusHandler getInstance(Class<?> cls);

    /**
     * Get a handler instance configured for a specific class and a known
     * source.
     */
    public IUFStatusHandler getInstance(Class<?> cls, String source);

    /**
     * Get a monitor handler instance configured for a specific class.
     */
    public IUFStatusHandler getMonitorInstance(Class<?> cls);

    /**
     * Get a monitor handler instance configured for a specific class and a
     * known source.
     */
    public IUFStatusHandler getMonitorInstance(Class<?> cls,
            String monitorSource);

    /**
     * Get a monitor handler instance configured for a known pluginId and a
     * known source.
     */
    public IUFStatusHandler getInstance(String pluginId, String source);

    /**
     * Get a monitor handler instance configured for a specific class and a
     * known category and source.
     */
    public IUFStatusHandler getInstance(Class<?> cls, String category,
            String source);

    /**
     * Get a monitor handler instance configured for a known pluginId, a known
     * category, and a known source.
     */
    public IUFStatusHandler getInstance(String pluginId, String category,
            String source);

    public IUFStatusHandler createInstance(AbstractHandlerFactory factory,
            String pluginId, String category);

    public void log(Priority priority, StatusHandler statusHandler,
            String message, Throwable throwable);

}