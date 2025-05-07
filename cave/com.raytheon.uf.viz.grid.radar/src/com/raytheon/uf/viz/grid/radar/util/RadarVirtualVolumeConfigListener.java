/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.grid.radar.util;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.util.ResourceUtil;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * This listens for virtual volumes to be toggled on/off, and updates resources
 * accordingly.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class RadarVirtualVolumeConfigListener implements IRadarConfigListener {

    /**
     * Executor for processing config changes in a background thread, so that
     * toggling virtual volumes doesn't freeze the UI at all.
     */
    private final ExecutorService executor = Executors
            .newSingleThreadExecutor(new ThreadFactoryBuilder()
                    .setNameFormat(getClass().getSimpleName() + "-%d").build());

    private final AbstractVizResource<?, ?> rsc;

    protected boolean virtualVolumeEnabled;

    protected RadarVirtualVolumeConfigListener(AbstractVizResource<?, ?> rsc) {
        this.rsc = rsc;
        virtualVolumeEnabled = RadarDisplayManager.getInstance()
                .getCurrentSettings().isVirtualVolumeEnabled();
    }

    @Override
    public void updateConfig() {
        executor.submit(() -> {
            updateConfigInternal();
        });
    }

    protected void updateConfigInternal() {
        boolean virtualVolumeEnabled = RadarDisplayManager.getInstance()
                .getCurrentSettings().isVirtualVolumeEnabled();
        if (virtualVolumeEnabled == this.virtualVolumeEnabled) {
            return;
        }

        this.virtualVolumeEnabled = virtualVolumeEnabled;
        AbstractResourceData rscData = rsc.getResourceData();
        if (rscData instanceof AbstractRequestableResourceData) {
            /*
             * Toggling virtual volumes can also toggle the availability of
             * frames for the latest scan time, if the latest scan doesn't have
             * enough tilts yet to display by itself.
             */
            ((AbstractRequestableResourceData) rscData)
                    .invalidateAvailableTimesCache();
        }
        RadarVirtualVolumeUtil.reloadFrames(ResourceUtil.getLatestTimes(rsc),
                rsc);

    }
}
