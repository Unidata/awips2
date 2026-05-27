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
package com.raytheon.uf.viz.grid.radar;

import com.raytheon.uf.common.time.DataTime;

/**
 * Contains info about which scans and tilts should be blended together when
 * creating a radar virtual volume. Higher tilts from the previous volume scan
 * should be blended into the current volume scan.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2024 2037092    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class VirtualVolumeInfo {

    private final DataTime latestVolumeScan;

    private final DataTime prevVolumeScan;

    private final double latestTilt;

    public VirtualVolumeInfo(DataTime latestVolumeScan, DataTime prevVolumeScan,
            double latestTilt) {
        this.latestVolumeScan = latestVolumeScan;
        this.prevVolumeScan = prevVolumeScan;
        this.latestTilt = latestTilt;
    }

    /**
     * @return latest/current volume scan time
     */
    public DataTime getLatestVolumeScan() {
        return latestVolumeScan;
    }

    /**
     * @return previous volume scan time
     */
    public DataTime getPrevVolumeScan() {
        return prevVolumeScan;
    }

    /**
     * Get the latest/highest tilt of the current volume scan. Only previous
     * volume scan tilts higher than this should be blended into the current
     * volume scan.
     *
     * @return latest/highest tilt of current volume scan.
     */
    public double getLatestTilt() {
        return latestTilt;
    }

    @Override
    public String toString() {
        return "VirtualVolumeInfo [latestVolumeScan=" + latestVolumeScan
                + ", prevVolumeScan=" + prevVolumeScan + ", latestTilt="
                + latestTilt + "]";
    }
}