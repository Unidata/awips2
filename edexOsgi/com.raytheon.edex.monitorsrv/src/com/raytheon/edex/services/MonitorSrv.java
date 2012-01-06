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
package com.raytheon.edex.services;

import com.raytheon.edex.monitors.IEdexMonitor;
import com.raytheon.uf.edex.core.EdexException;

/**
 * This is a service endpoint for EDEX monitoring. It receives an
 * {@link IEdexMonitor} instance by injection and calls the
 * {@link IEdexMonitor#execute()} method. All error handling is managed here as
 * well.
 * <P>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02May2008    1113       MW Fegan    Initial implementation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class MonitorSrv {
    private IEdexMonitor edexMonitor = null;

    /**
     * Constructor. Creates an empty end-point ready for use by Mule.
     */
    public MonitorSrv() {
        // Intentionally empty.
    }

    public void process() throws EdexException {
        if (edexMonitor == null) {
            throw new EdexException("Monitor is null - unable to process");
        }
        edexMonitor.execute();
        return;
    }

    /**
     * Returns the {@link IEdexMonitor} instance used by this end-point.
     */
    public IEdexMonitor getEdexMonitor() {
        return edexMonitor;
    }

    /**
     * Sets the {@link IEdexMonitor} instance for the monitor to use.
     * <P>
     * Note: This is provided to allow dependency injection by the container.
     */
    public void setEdexMonitor(IEdexMonitor edexMonitor) {
        this.edexMonitor = edexMonitor;
    }

}
