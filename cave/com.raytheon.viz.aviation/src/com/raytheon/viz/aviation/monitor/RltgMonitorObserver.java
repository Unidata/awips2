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

import java.util.Collection;

import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Watches incoming bufrmosalerts to see if they are LAMP and also apply to a
 * configured site, and if so, triggers an event to run the monitoring rules for
 * that site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class RltgMonitorObserver extends MonitorObserver {

    public RltgMonitorObserver(TafMonitorDlg dlg) {
        super(dlg);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(com.raytheon.viz.
     * alerts.IAlertObserver.AlertMessage[])
     */
    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        for (AlertMessage alert : alertMessages) {
            String station = (String) alert.decodedAlert
                    .get("location.stationId");
            for (TafSiteComp tsc : dialog.getTafSiteComps()) {
                if (tsc.getStationName().equals(station)) {
                    String msg = "Checking rltg for " + station;
                    statusMessage(msg);
                    tsc.fireMonitor("RadLtgMonitor");
                }
            }
        }

    }

}