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
package com.raytheon.viz.alerts;

import java.util.Collection;

import com.raytheon.uf.viz.core.alerts.AlertMessage;

/**
 * 
 * Provides generalized interface into receiving CAVE alert messages
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/28/2008   966        chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IAlertObserver {

    /**
     * Indicates that an alert message has arrived.
     * 
     * This method will be invoked in thread that has been tied to the listener.
     * Thus, it is possible to perform longer-running operations inside
     * implementations of this method. Note that only one thread will be
     * allocated per registration and throughput is potentially throttled per
     * listener.
     * 
     * @param alertMessages
     *            an array of alert messages containing both the original
     *            dataURI message and the decoded equivalent
     */
    public void alertArrived(Collection<AlertMessage> alertMessages);
}
