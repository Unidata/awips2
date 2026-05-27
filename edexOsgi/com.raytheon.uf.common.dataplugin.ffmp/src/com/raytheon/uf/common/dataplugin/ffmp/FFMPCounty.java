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
package com.raytheon.uf.common.dataplugin.ffmp;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPCounties.CountySort;
import org.locationtech.jts.geom.Geometry;

/**
 * County
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * 12 Feb, 2010  3915     dhladky   Initial creation
 * Aug 07, 2018  6642     randerso  Added geometry field. Removed setters and
 *                                  default constructor that were unused.
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFMPCounty implements Comparable<FFMPCounty> {
    private String countyName = null;

    private String state;

    private Long gid;

    private String countyFips;

    private String displayFips;

    private String displayCountyName;

    private Geometry geometry;

    /**
     * Constructor
     *
     * @param gid
     * @param countyName
     * @param countyFips
     * @param state
     * @param geometry
     */
    public FFMPCounty(Long gid, String countyName, String countyFips,
            String state, Geometry geometry) {
        this.countyName = countyName;
        this.state = state;
        this.gid = gid;
        this.countyFips = countyFips;
        this.geometry = geometry;

        // Create the display for the county ID list.
        StringBuilder sb = new StringBuilder(state);
        sb.append("C").append(this.countyFips.substring(2));
        this.displayFips = sb.toString();

        // Create the display string for the county name list.
        sb = new StringBuilder(state).append(",").append(this.countyName);
        this.displayCountyName = sb.toString();
    }

    /**
     * @return the countyName
     */
    public String getCountyName() {
        return countyName;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @return the gid
     */
    public Long getGid() {
        return gid;
    }

    /**
     * @return the countyFips
     */
    public String getCountyFips() {
        return countyFips;
    }

    /**
     * @return the displayFips
     */
    public String getDisplayFips() {
        return displayFips;
    }

    /**
     * @return the displayCountyName
     */
    public String getDisplayCountyName() {
        return displayCountyName;
    }

    /**
     * @return the geometry
     */
    public Geometry getGeometry() {
        return geometry;
    }

    @Override
    public int compareTo(FFMPCounty obj) {

        if (obj != null) {

            if (FFMPCounties.getSortBy() == CountySort.NAME) {
                return this.countyName.compareTo(obj.getCountyName());
            } else if (FFMPCounties.getSortBy() == CountySort.ID) {
                return this.countyFips.compareTo(obj.getCountyFips());
            } else if (FFMPCounties.getSortBy() == CountySort.DISPLAY_NAME) {
                return this.displayCountyName
                        .compareTo(obj.getDisplayCountyName());
            }
        }

        return 0;
    }

    @Override
    public String toString() {
        return countyName;
    }
}
