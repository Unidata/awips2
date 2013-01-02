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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.Activator;
import com.raytheon.viz.hydrocommon.constants.StatusConstants;
import com.raytheon.viz.hydrocommon.radaroverlay.RadarRingOverlayData;
import com.raytheon.viz.hydrocommon.resource.RadarRingOverlayResource;
import com.raytheon.viz.hydrocommon.resource.RadarRingOverlayResourceData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * MPE Radar Rings Overlay Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 22, 2010 4356       mpduff      Initial creation.
 * Apr 4, 2011  8934        mnash       Fix memory leaks, added timer to retrieve data
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class MPERadarRingOverlayResource extends RadarRingOverlayResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(MPERadarRingOverlayResource.class);
    // utilizes the RadarRingOverlayResource, except adds an extra color and
    // also adds an extra query
    private final RGB RED = new RGB(255, 0, 0);

    public MPERadarRingOverlayResource(RadarRingOverlayResourceData rscData,
            LoadProperties loadProps) {
        super(rscData, loadProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrocommon.resource.RadarRingOverlayResource#
     * constructDataTimer()
     */
    @Override
    protected void constructDataTimer() {
        timer = new Timer("mperadarRingRetrieve");
        timerTask = new TimerTask() {
            @Override
            public void run() {
                try {
                    // same query as in RadarRingOverlayResource, but done here
                    // to prevent thread issues
                    dataMap = dao.getData();
                    if (dataMap != null) {
                        // Query for ring color, green if radar data
                        // available for that hour, red if not available
                        MPEDisplayManager displayManager = MPEDisplayManager
                                .getCurrent();
                        if (displayManager != null) {
                            Date displayDate = displayManager.getCurrentEditDate();
                            for (RadarRingOverlayData rdata : dataMap.values()) {
                                dao.getRadarAvailable(rdata, displayDate);
                                if (rdata.isRadAvail()) {
                                    rdata.setColor(GREEN);
                                } else {
                                    rdata.setColor(RED);
                                }
                            }
                            loadDone = true;
                            issueRefresh();
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to retrieve the data", e);

                }
            }
        };
        // update data every 30 seconds
        timer.schedule(timerTask, 0, 30000);
    }
}
