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
package com.raytheon.viz.aviation.monitor;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Base observer class for monitoring data for the monitoring dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2009              njensen     Initial creation
 * 19Mar2014    #2925       lvenable    Added dispose checks for runAsync.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class MonitorObserver implements IAlertObserver {

    protected final static RGB GREEN = RGBColors.getRGBColor("green");

    protected TafMonitorDlg dialog;

    protected MonitorObserver(TafMonitorDlg dlg) {
        dialog = dlg;
    }

    protected void statusMessage(final String msg) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (dialog.isDisposed() == false) {
                    dialog.getMessageBar().setMessageText(msg, GREEN);
                }
            }
        });
    }

}
