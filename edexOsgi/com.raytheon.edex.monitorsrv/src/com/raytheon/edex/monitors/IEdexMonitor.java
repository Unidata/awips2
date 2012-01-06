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
package com.raytheon.edex.monitors;

import com.raytheon.edex.services.MonitorSrv;

/**
 * Specifies the interface defining the capabilities of the EDEX monitor
 * classes. These are used by the {@link MonitorSrv} to monitor and report
 * various system conditions.
 *  
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02May2008    TBD        MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public interface IEdexMonitor {
    /**
     * Sets the user data from the monitor. This should be called just
     * prior to running the {@link #execute()} method.
     * @param data the data to provide to the monitor
     */
    public void setData(final String data);
    /**
     * Triggers a single monitoring event in the monitor.
     */
    public void execute();
}
