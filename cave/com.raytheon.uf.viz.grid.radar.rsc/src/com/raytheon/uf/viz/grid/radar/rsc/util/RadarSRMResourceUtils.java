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
package com.raytheon.uf.viz.grid.radar.rsc.util;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * Utilities for radar Storm Relative Velocity Map (SRM) resources.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2023 9021       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarSRMResourceUtils {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarSRMResourceUtils.class);

    /**
     * Private constructor to prevent instantiation since everything's static.
     */
    private RadarSRMResourceUtils() {
    }

    /**
     * Register listeners for the given radar SRM resource, so that it updates
     * when SRM configuration changes.
     *
     * @param rsc
     *            the radar SRM resource to register listeners for
     */
    public static void registerSRMListeners(AbstractVizResource<?, ?> rsc) {
        IRadarConfigListener radarConfigListener = () -> handleConfigUpdate(
                rsc);
        IToolChangedListener toolListener = () -> handleConfigUpdate(rsc);

        RadarDisplayManager.getInstance().addListener(radarConfigListener);
        ToolsDataManager.getInstance()
                .addStormTrackChangedListener(toolListener);

        rsc.registerListener((IDisposeListener) resource -> {
            RadarDisplayManager.getInstance()
                    .removeListener(radarConfigListener);
            ToolsDataManager.getInstance()
                    .removeStormTrackChangedListener(toolListener);
        });
    }

    private static void handleConfigUpdate(AbstractVizResource<?, ?> rsc) {
        for (DataTime dt : rsc.getDataTimes()) {
            rsc.getResourceData().fireChangeListeners(ChangeType.DATA_REMOVE,
                    dt);
        }

        IDescriptor desc = rsc.getDescriptor();
        AbstractTimeMatcher timeMatcher = desc.getTimeMatcher();
        timeMatcher.redoTimeMatching(rsc);
        try {
            timeMatcher.redoTimeMatching(desc);
        } catch (VizException e) {
            statusHandler.error(
                    "Error updating radar SRM resource for configuration change: "
                            + rsc,
                    e);
        }
    }
}
