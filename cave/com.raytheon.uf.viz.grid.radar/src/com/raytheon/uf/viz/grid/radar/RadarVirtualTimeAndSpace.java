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
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.time.DataTime;

/**
 * Extension of {@link TimeAndSpace} for representing radar data from the
 * previous volume scan as being available for the current volume scan time.
 *
 * This is used to support the radar virtual volume concept. An example is Radar
 * Reflectivity at 1kft AGL, which is derived from the raw radar data at various
 * elevation angles. The parameter may be derived using the real 0.5-1.5 angle
 * data from the current 12:12Z volume scan, along with the 2.4-3.4 angles from
 * the previous 12:06Z scan. The 0.5 and 1.5 angle data records would each have
 * normal {@link TimeAndSpace} values, while the 2.4 and 3.4 angles would each
 * have instances of this class with time=12:12Z and prevScanTime=12:06Z.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 * Jul 15, 2024 2037624    mapeters    Made radar-specific
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarVirtualTimeAndSpace extends TimeAndSpace {

    protected final DataTime prevScanTime;

    /**
     * Constructor.
     *
     * @param time
     *            the time to claim the data is for, which is the current volume
     *            scan time
     * @param space
     *            the data's space
     * @param prevScanTime
     *            the previous volume scan time, which the data is actually from
     */
    public RadarVirtualTimeAndSpace(DataTime time, IGridGeometryProvider space,
            DataTime prevScanTime) {
        super(time, space);
        this.prevScanTime = prevScanTime;
    }

    /**
     * Get the previous scan time, which the data is actually from.
     *
     * @return previous scan time
     */
    public DataTime getPrevScanTime() {
        return prevScanTime;
    }

    @Override
    public boolean isVirtual() {
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(prevScanTime);
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
        RadarVirtualTimeAndSpace other = (RadarVirtualTimeAndSpace) obj;
        return Objects.equals(prevScanTime, other.prevScanTime);
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
        return sb.toString();
    }
}
