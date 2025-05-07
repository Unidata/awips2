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

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Set;
import java.util.function.Supplier;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * Utility class for working with radar virtual volumes. A virtual volume is
 * when higher tilt data from the previous volume scan is blended into the
 * current volume scan, in order to produce a full volume scan of the latest
 * data at each tilt.
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
public class RadarVirtualVolumeUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarVirtualVolumeUtil.class);

    private static final ThreadLocal<SimpleDateFormat> PREV_SCAN_FORMAT = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat("HH:mm'Z'");
            sdf.setTimeZone(TimeUtil.GMT_TIME_ZONE);
            return sdf;
        }
    };

    /**
     * Private constructor to prevent instantiation since everything is static.
     */
    private RadarVirtualVolumeUtil() {
    }

    /**
     * Register listeners for the given resource so that its virtual volume
     * status stays up-to-date as data updates come in and as virtual volumes
     * are toggled on/off.
     *
     * @param rsc
     *            virtual volume resource to register listeners for
     * @param rscPdoSupplier
     *            function for getting all the resource's data records at any
     *            given time
     */
    public static void registerVirtualVolumeListeners(
            AbstractVizResource<?, ?> rsc,
            Supplier<Collection<? extends PluginDataObject>> rscPdoSupplier) {
        IRadarConfigListener radarConfigListener = new RadarVirtualVolumeConfigListener(
                rsc);
        rsc.registerListener((IDisposeListener) resource -> RadarDisplayManager
                .getInstance().removeListener(radarConfigListener));
        RadarDisplayManager.getInstance().addListener(radarConfigListener);

        rsc.registerListener(
                new RadarVirtualVolumeRefreshListener(rsc, rscPdoSupplier));
    }

    /**
     * Reload frames for a resource.
     *
     * @param frameTimes
     *            the frame times to reload
     * @param resource
     *            the resource
     */
    public static void reloadFrames(Set<DataTime> frameTimes,
            AbstractVizResource<?, ?> resource) {
        if (resource == null) {
            return;
        }
        AbstractResourceData rscData = resource.getResourceData();
        IDescriptor descriptor = resource.getDescriptor();
        if (rscData == null || descriptor == null) {
            return;
        }
        if (!frameTimes.isEmpty()) {
            for (DataTime time : frameTimes) {
                rscData.fireChangeListeners(ChangeType.DATA_UPDATE, time);
            }
            /*
             * Tell the time matcher to update this resource's time info
             * whenever time matching is re-done
             */
            descriptor.getTimeMatcher().redoTimeMatching(resource);
            try {
                // Actually re-do the time matching
                descriptor.redoTimeMatching();
            } catch (VizException e) {
                statusHandler.error(
                        "Error updating virtual volume for: " + resource, e);
            }
        }
    }

    /**
     * Build virtual volume status text to display in a product's legend.
     *
     * @param currScanTilt
     *            latest/highest tilt of current volume scan
     * @param prevScanTime
     *            time of previous scan that higher tilts are blended in from
     *
     * @return virtual volume status legend text
     */
    public static String buildLegendText(Double currScanTilt,
            DataTime prevScanTime) {
        if (prevScanTime == null || currScanTilt == null) {
            return "";
        }
        StringBuilder sb = new StringBuilder("(");
        sb.append(PREV_SCAN_FORMAT.get()
                .format(prevScanTime.getValidTimeAsDate()));
        sb.append(" above ").append(currScanTilt).append(")");
        return sb.toString();
    }

    /**
     * Build virtual volume status text to display in a product's legend.
     *
     * @param status
     *            product's virtual volume status
     *
     * @return virtual volume status legend text
     */
    public static String buildLegendText(RadarVirtualVolumeStatus status) {
        if (status == null) {
            return "";
        }
        return buildLegendText(status.getCurrScanTilt(),
                status.getPrevScanTime());
    }
}
