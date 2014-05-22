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
package com.raytheon.uf.edex.core;

/**
 * Methods to be called as part of the context life cycle for starting and
 * stopping the context.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2726       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public interface IContextStateProcessor {

    /**
     * Perform any work that needs to be done before the context is started,
     * such as initialization.
     */
    public void preStart();

    /**
     * Perform any work that needs to be done after the context is started, such
     * as sending notifications to clients.
     */
    public void postStart();

    /**
     * Perform any work that needs to be done before context is stopped, such as
     * notifying async threads to stop.
     */
    public void preStop();

    /**
     * Perform any work that needs to be done after the context is stopped, such
     * as sending in memory data.
     */
    public void postStop();
}
