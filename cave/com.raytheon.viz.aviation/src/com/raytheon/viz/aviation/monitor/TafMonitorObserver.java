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

import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.guidance.GuidanceRequest;
import com.raytheon.viz.aviation.guidance.GuidanceRequest.GuidanceType;
import com.raytheon.viz.aviation.guidance.PythonGuidanceJob;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Watches incoming TAF alerts to see if they apply to a configured site, and if
 * so, triggers an event to run the monitoring rules for that site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2,  2009            njensen     Initial creation
 * Nov 12, 2010 6195       rferrel     Clear guidance's TAF cache when
 *                                     an alert received.
 * May 15, 2014 3002       bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TafMonitorObserver extends MonitorObserver implements
        IRequestCompleteListener<String[]> {

    public TafMonitorObserver(TafMonitorDlg dlg) {
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
            String station = (String) alert.decodedAlert.get("stationId");
            for (TafSiteComp tsc : dialog.getTafSiteComps()) {
                if (tsc.getStationName().equals(station)) {
                    siteIDs.add(station);
                    break;
                }
            }
        }

        if (siteIDs.size() > 0) {
            // Clear Python's TAF cache for siteIDs.
            GuidanceRequest request = new GuidanceRequest();
            request.setGuidanceType(GuidanceType.TAF_CACHE);
            request.setSiteIDs(siteIDs);
            request.setListener(this);
            PythonGuidanceJob.getInstance().enqueue(request);
        }
    }

    @Override
    public void requestComplete(String[] result) {
        // TAF cache cleared proceed with the notifications.
        for (String station : result) {
            String msg = "Checking tafs for " + station;
            statusMessage(msg);
            for (TafSiteComp tsc : dialog.getTafSiteComps()) {
                if (tsc.getStationName().equals(station)) {
                    // TODO should retrieve based on dataUri
                    TafRecord taf = TafUtil.getLatestTaf(station);
                    tsc.tafArrived(taf);
                    break;
                }
            }
        }
    }
}
