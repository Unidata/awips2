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

import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.grid.radar.RadarVirtualTimeAndSpace;

/**
 * Indicates the radar virtual volume status for a single radar volume scan
 * time.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2024 2037624    mapeters    Initial creation
 * Aug 14, 2024 2037631    mapeters    Add currScanTime field, throw exception
 *                                     for non-TILT data records
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarVirtualVolumeStatus {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarVirtualVolumeStatus.class);

    private final DataTime currScanTime;

    private final double currScanTilt;

    private final DataTime prevScanTime;

    protected RadarVirtualVolumeStatus(DataTime currScanTime,
            double currScanTilt, DataTime prevScanTime) {
        this.currScanTime = currScanTime;
        this.currScanTilt = currScanTilt;
        this.prevScanTime = prevScanTime;
    }

    /**
     * @return current volume scan time
     */
    public DataTime getCurrScanTime() {
        return currScanTime;
    }

    /**
     * @return latest/highest tilt of current volume scan
     */
    public double getCurrScanTilt() {
        return currScanTilt;
    }

    /**
     * @return time of previous scan that higher tilts are blended in from
     */
    public DataTime getPrevScanTime() {
        return prevScanTime;
    }

    /**
     * Build an instance of this class indicating the virtual volume status of
     * the given scan time, based on the given data.
     *
     * @param dataList
     *            list including data for the given time (may include other
     *            times as well, which will be ignored)
     * @param time
     *            the scan time to get the virtual volume status for
     * @return virtual volume status if the given time/data indicate that a
     *         virtual volume is currently being used, null otherwise
     */
    public static RadarVirtualVolumeStatus build(
            List<AbstractRequestableData> dataList, DataTime time) {
        Double maxNormalTilt = null;
        Double minVirtualTilt = null;
        DataTime prevScanTime = null;
        for (AbstractRequestableData data : dataList) {
            TimeAndSpace tas = data.getTimeAndSpace();
            // Ignore spatial, time arg may have frame level set on it
            if (!tas.getTime().equals(time, true)) {
                continue;
            }
            String levelName = data.getLevel().getMasterLevel().getName();
            if (!RadarUtil.TILT.equals(levelName)) {
                throw new RuntimeException(
                        "Radar virtual volumes can only be derived from data with 'TILT' levels, not '"
                                + levelName + "'");
            }
            double tilt = data.getLevel().getLevelonevalue();
            if (tas instanceof RadarVirtualTimeAndSpace) {
                if (minVirtualTilt == null || tilt < minVirtualTilt) {
                    minVirtualTilt = tilt;
                }
                DataTime realTime = ((RadarVirtualTimeAndSpace) tas)
                        .getPrevScanTime();
                if (prevScanTime == null) {
                    prevScanTime = realTime;
                } else if (!prevScanTime.equals(realTime)) {
                    /*
                     * This should never happen based on the derived parameter
                     * logic
                     */
                    statusHandler.error(
                            "Multiple previous scan times used in virtual volume for "
                                    + time + ": [" + prevScanTime + ", "
                                    + realTime + "]");
                }
            } else {
                if (maxNormalTilt == null || tilt > maxNormalTilt) {
                    maxNormalTilt = tilt;
                }
            }
        }

        if (minVirtualTilt != null && maxNormalTilt != null
                && minVirtualTilt <= maxNormalTilt) {
            // This should never happen based on the derived parameter logic
            statusHandler.error("Virtual volume for " + time + " includes "
                    + minVirtualTilt
                    + " from previous scan, which is lower than current scan tilt "
                    + maxNormalTilt);
        }

        if (maxNormalTilt != null && prevScanTime != null) {
            time = time.clone();
            time.clearLevel();
            return new RadarVirtualVolumeStatus(time, maxNormalTilt,
                    prevScanTime);
        }
        return null;
    }
}
