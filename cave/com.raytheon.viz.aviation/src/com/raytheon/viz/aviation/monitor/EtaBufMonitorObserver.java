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

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.aviation.guidance.EtaViewer;
import com.raytheon.viz.aviation.guidance.ViewerTab;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Watches incoming ETA buf (modelsounding) alerts to see if they apply to any
 * configured site; if so clears cache and updates viewer model tabs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2011 11612      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class EtaBufMonitorObserver extends MonitorObserver implements
        IAlertObserver {

    public static String pluginName = "modelsounding";

    public EtaBufMonitorObserver(TafMonitorDlg dlg) {
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
			DataTime dataTime = (DataTime) alert.decodedAlert.get("dataTime");
			if (dataTime.getRefTime().before(
					SimulatedTime.getSystemTime().getTime())) {

				String siteID = (String) alert.decodedAlert
						.get("location.stationId");
				if (!siteIDs.contains(siteID)) {
					for (TafSiteComp tsc : dialog.getTafSiteComps()) {
						if (tsc.getStationName().equals(siteID)) {
							siteIDs.add(siteID);
							break;
						}
					}
				}
			}
		}

        if (siteIDs.size() > 0) {
            for (ViewerTab tab : dialog.getViewerTabList()) {
                if (tab instanceof com.raytheon.viz.aviation.guidance.EtaViewer) {
                    EtaViewer eta = (EtaViewer) tab;
                    if ("etabuf".equals(eta.getModel())) {
                        tab.alertSites(siteIDs);
                    }
                }
            }
        }
    }
}
