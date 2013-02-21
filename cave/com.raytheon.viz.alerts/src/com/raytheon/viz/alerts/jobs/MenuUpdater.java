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
package com.raytheon.viz.alerts.jobs;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.rsc.URICatalog;
import com.raytheon.uf.viz.core.rsc.URICatalog.IURIRefreshCallback;
import com.raytheon.viz.alerts.IAlertObserver;

/**
 * Handles updating menu times
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/28/2008   966        chammack    Split from AlertManagerJob
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class MenuUpdater implements IAlertObserver {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(java.lang.String,
     * java.util.Map)
     */
    @Override
    public void alertArrived(final Collection<AlertMessage> alertMessages) {
        for (AlertMessage message : alertMessages) {
            Map<String, Object> attribs = new HashMap<String, Object>(
                    message.decodedAlert);
            DataTime dataTimeStr = (DataTime) attribs.get("dataTime");

            if (dataTimeStr != null) {
                // Update the date catalog
                List<List<IURIRefreshCallback>> dataTimeList = URICatalog
                        .getInstance().searchTree(attribs);

                // If we found listeners, update them
                if (dataTimeList != null) {
                    DataTime dataTime = dataTimeStr.clone();

                    for (List<IURIRefreshCallback> refreshList : dataTimeList) {
                        for (IURIRefreshCallback callback : refreshList) {
                            if (callback != null) {
                                callback.updateTime(dataTime);
                            }
                        }
                    }
                }
            }
        }
    }

}
