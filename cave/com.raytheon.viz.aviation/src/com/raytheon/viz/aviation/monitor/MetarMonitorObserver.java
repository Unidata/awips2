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

import java.util.ArrayList;
import java.util.Collection;

import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.guidance.GuidanceRequest;
import com.raytheon.viz.aviation.guidance.GuidanceRequest.GuidanceType;
import com.raytheon.viz.aviation.guidance.PythonGuidanceJob;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Watches incoming metar (obs) alerts to see if they apply to a configured
 * site, and if so, triggers an event to run the monitoring rules for that site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2009            njensen     Initial creation
 * Nov 12,2010  6195      rferrel     Clear guidance metar cache
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MetarMonitorObserver extends MonitorObserver implements
        IRequestCompleteListener<String[]> {

    public MetarMonitorObserver(TafMonitorDlg dlg) {
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
        ArrayList<String> siteIDs = new ArrayList<String>();

        for (AlertMessage alert : alertMessages) {
            String siteID = (String) alert.decodedAlert
                    .get("location.stationId");
            for (TafSiteComp tsc : dialog.getTafSiteComps()) {
                if (tsc.getStationName().equals(siteID)) {
                    siteIDs.add(siteID);
                    break;
                }
            }
        }

        if (siteIDs.size() > 0) {
            // Clear the python METAR cache
            GuidanceRequest request = new GuidanceRequest();
            request.setGuidanceType(GuidanceType.METAR_CACHE);
            request.setSiteIDs(siteIDs);
            request.setListener(this);
            PythonGuidanceJob.getInstance().enqueue(request);
        }
    }

    @Override
    public void requestComplete(String[] result) {
        // METAR Cache is cleared proceed with the notifications
        for (String siteID : result) {
            String msg = "checking mtrs for " + siteID;
            statusMessage(msg);
            for (TafSiteComp tsc : dialog.getTafSiteComps()) {
                if (tsc.getStationName().equals(siteID)) {
                    tsc.fireMonitor("MetarMonitor");
                    break;
                }
            }
        }
    }
}
