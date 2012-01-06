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
package com.raytheon.viz.hydro.resource;

import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.QueryMode;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Force the selected resource to refresh.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2010  4104       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ResourceRefresh {
    /**
     * Refresh the MultiPointResource.
     */
    public static void RefreshMultiPointResource() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        MultiPointResource mpr = pdcManager.getMultiPointResource();

        if ((mpr != null) && !mpr.isDisposed()) {
            if (pcOptions.getQueryMode() == QueryMode.AD_HOC_MODE
                    .getQueryMode()) {
                pdcManager.scheduleRequest(true,
                        PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);
            } else {
                pdcManager.scheduleRequest(true,
                        PointDataControlManager.REQUEST_TYPE.REQUEST_TIME_STEP);
            }

            pdcManager.applyShiftValues();
        }
    }

    public static void RefreshDamDisplayResource() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        DamLocationResource dlr = pdcManager.getDamLocationResource();

        if (dlr != null) {
            dlr.issueRefresh();
        }

    }
}
