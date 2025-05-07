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

import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.util.ResourceUtil;
import com.raytheon.uf.viz.grid.radar.RadarVirtualTimeAndSpace;
import com.raytheon.viz.grid.record.RequestableDataRecord;

/**
 * This listens for a radar virtual volume resource to be refreshed, and
 * performs any necessary actions.
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
class RadarVirtualVolumeRefreshListener implements IRefreshListener {

    /**
     * Executor service for handling the refresh in a background thread. The
     * refresh listener stack overflows without this, since it triggers more
     * refreshes itself.
     */
    private final ExecutorService executor = Executors
            .newSingleThreadExecutor(new ThreadFactoryBuilder()
                    .setNameFormat(getClass().getSimpleName() + "-%d").build());

    private final AbstractVizResource<?, ?> rsc;

    private final Supplier<Collection<? extends PluginDataObject>> pdoSupplier;

    /**
     * Create a listener for processing a refresh of the given radar virtual
     * volume resource.
     *
     * @param rsc
     *            radar virtual volume resource
     * @param rscPdoSupplier
     *            function for getting all the resource's data records at any
     *            given time
     */
    public RadarVirtualVolumeRefreshListener(AbstractVizResource<?, ?> rsc,
            Supplier<Collection<? extends PluginDataObject>> rscPdoSupplier) {
        this.rsc = rsc;
        this.pdoSupplier = rscPdoSupplier;
    }

    @Override
    public void refresh() {
        executor.submit(() -> {
            refreshInternal();
        });
    }

    protected void refreshInternal() {
        /*
         * Virtual volume should only be used for the latest volume scan. Reload
         * any earlier frames that are using virtual volumes, which is possible
         * if an incomplete volume scan comes in, and then the next volume scan
         * starts. Underlying derived parameters should only use virtual volumes
         * for the latest volume scan, so the reloaded data will be correct.
         */
        DataTime latestTime = ResourceUtil.getLatestTimeNonSpatial(rsc);
        if (latestTime == null) {
            return;
        }
        Set<DataTime> virtualRecordTimes = pdoSupplier.get().stream()
                .filter(record -> record instanceof RequestableDataRecord
                        && ((RequestableDataRecord) record)
                                .getTimeAndSpace() instanceof RadarVirtualTimeAndSpace)
                .map(PluginDataObject::getDataTime).collect(Collectors.toSet());

        Set<DataTime> nonLatestVirtualFrameTimes = Arrays
                .stream(rsc.getDataTimes())
                .filter(dt -> !dt.equals(latestTime, true)
                        && virtualRecordTimes.stream().anyMatch(
                                virtTime -> virtTime.equals(dt, true)))
                .collect(Collectors.toSet());
        RadarVirtualVolumeUtil.reloadFrames(nonLatestVirtualFrameTimes, rsc);
    }
}
