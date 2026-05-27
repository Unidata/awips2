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

import java.util.Objects;

import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.time.DataTime;

/**
 * Extension of {@link RadarVirtualTimeAndSpace} for data that is derived from a
 * combination of current volume scan data and previous volume scan data.
 *
 * This is used to support the radar virtual volume concept in derived
 * parameters. An example is Radar Reflectivity at 1kft AGL, which is derived
 * from the raw radar data at various elevation angles. The parameter may be
 * derived using the real 0.5-1.5 angle data from the current 12:12Z volume
 * scan, along with the 2.5-3.4 angles from the previous 12:06Z scan. For that
 * derived data, this class would have time=12:12Z, prevScanTime=12:06Z, and
 * currScanTilt=1.5.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarVirtualDerivedTimeAndSpace extends RadarVirtualTimeAndSpace {

    private final double currScanTilt;

    /**
     * Constructor.
     *
     * @param time
     *            the time to represent the data as being available for, which
     *            is the current volume scan time
     * @param space
     *            space where the data can exist
     * @param prevScanTime
     *            the previous volume scan time, which some of the data
     *            dependencies are actually from
     * @param currScanTilt
     *            the elevation angle that the current volume scan is at; higher
     *            elevation angles are from the previous volume scan
     */
    public RadarVirtualDerivedTimeAndSpace(DataTime time,
            IGridGeometryProvider space, DataTime prevScanTime,
            double currScanTilt) {
        super(time, space, prevScanTime);
        this.currScanTilt = currScanTilt;
    }

    /**
     * Get the elevation angle that the current volume scan is at. Higher
     * elevation angles are from the previous volume scan.
     *
     * @return current volume scan's elevation angle
     */
    public double getCurrScanTilt() {
        return currScanTilt;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(currScanTilt);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RadarVirtualDerivedTimeAndSpace other = (RadarVirtualDerivedTimeAndSpace) obj;
        return Objects.equals(currScanTilt, other.currScanTilt);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("AvailSpT: Time: ");
        sb.append(getTime());
        sb.append(", Previous Scan Time: ");
        sb.append(prevScanTime);
        sb.append(", Space: ");
        sb.append(getSpace());
        sb.append(", Current Scan Tilt: ");
        sb.append(currScanTilt);
        return sb.toString();
    }
}
