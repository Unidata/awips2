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
package com.raytheon.uf.edex.esb.camel.context;

import org.apache.camel.CamelContext;
import org.apache.camel.Route;

/**
 * Represents a way for managing a context for starting and stopping. Allows for
 * Context with different purposes to be handled independently of each other.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2014 2726       rjpeter     Initial creation.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public interface IContextStateManager {
    /**
     * Is the {@code CamelContext} startable?
     * 
     * @param context
     * @return
     * @throws Exception
     */
    public boolean isContextStartable(CamelContext context) throws Exception;

    /**
     * Start the {@code CamelContext}.
     * 
     * @param context
     * @return
     * @throws Exception
     */
    public boolean startContext(CamelContext context) throws Exception;

    /**
     * Start the {@code Route}.
     * 
     * @param context
     * @return
     * @throws Exception
     */
    public boolean startRoute(Route route) throws Exception;

    /**
     * Is the {@code CamelContext} stoppable?
     * 
     * @param context
     * @return
     * @throws Exception
     */
    public boolean isContextStoppable(CamelContext context) throws Exception;

    /**
     * Stop the {@code CamelContext}.
     * 
     * @param context
     * @return
     * @throws Exception
     */
    public boolean stopContext(CamelContext context) throws Exception;

    /**
     * Stop the {@code Route}.
     * 
     * @param context
     * @return
     * @throws Exception
     */
    public boolean stopRoute(Route route) throws Exception;
}