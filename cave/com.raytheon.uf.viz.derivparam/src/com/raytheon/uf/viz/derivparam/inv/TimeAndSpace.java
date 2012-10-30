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
package com.raytheon.uf.viz.derivparam.inv;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Represents a time and space(location) where data can exist. This is used in
 * derived parameters for data types to report when and where data can be
 * loaded.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class TimeAndSpace {

    /**
     * A constant to represent the time for data that does not change over time
     * or that is available for all times, an example would be topography which
     * generally does not change on a day to day basis.
     */
    public static final DataTime TIME_AGNOSTIC = new DataTime() {

        private static final long serialVersionUID = 1L;

        @Override
        public boolean equals(Object obj) {
            return this == obj;
        }

        @Override
        public String toString() {
            return "TIME_AGNOSTIC";
        }

    };

    /**
     * A constant to represent the space for data that does not change over
     * space or that is available for all spaces, an example would be radar
     * elevation angle for a grid coverage, this can be calculated for any
     * gridcoverage.
     */
    public static final ISpatialObject SPACE_AGNOSTIC = new ISpatialObject() {

        private static final long serialVersionUID = 1L;

        @Override
        public Integer getNy() {
            return null;
        }

        @Override
        public Integer getNx() {
            return null;
        }

        @Override
        public Geometry getGeometry() {
            return null;
        }

        @Override
        public CoordinateReferenceSystem getCrs() {
            return null;
        }

        @Override
        public String toString() {
            return "SPACE_AGNOSTIC";
        }

    };

    private final DataTime time;

    private final ISpatialObject space;

    public TimeAndSpace() {
        this(TIME_AGNOSTIC, SPACE_AGNOSTIC);
    }

    public TimeAndSpace(DataTime time) {
        this(time, SPACE_AGNOSTIC);

    }

    public TimeAndSpace(ISpatialObject space) {
        this(TIME_AGNOSTIC, space);
    }

    public TimeAndSpace(DataTime time, ISpatialObject space) {
        this.time = time;
        this.space = space;
    }

    public DataTime getTime() {
        return time;
    }

    public ISpatialObject getSpace() {
        return space;
    }

    public boolean isTimeAgnostic() {
        return this.time == TIME_AGNOSTIC;
    }

    public boolean isSpaceAgnostic() {
        return space == SPACE_AGNOSTIC;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((space == null) ? 0 : space.hashCode());
        result = prime * result + ((time == null) ? 0 : time.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TimeAndSpace other = (TimeAndSpace) obj;
        if (space == null) {
            if (other.space != null)
                return false;
        } else if (!space.equals(other.space))
            return false;
        if (time == null) {
            if (other.time != null)
                return false;
        } else if (!time.equals(other.time))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("AvailSpT: Time: ");
        sb.append(time);
        sb.append(", Space: ");
        sb.append(space);
        return sb.toString();
    }

}
