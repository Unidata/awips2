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

/**
 * County
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Feb, 2010 3915         dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPCounty implements Comparable<FFMPCounty> {
    private String countyName = null;

    private String state = null;

    private Long gid = null;

    private String countyFips = null;

    private String displayFips = null;

    private String displayCountyName = null;

    public FFMPCounty() {
    }

    public FFMPCounty(Long gid, String countyName, String countyFips,
            String state) {
        this.countyName = countyName;
        this.state = state;
        this.gid = gid;
        this.countyFips = countyFips;

        // Create the display for the county ID list.
        StringBuilder sb = new StringBuilder(state);
        sb.append("C").append(this.countyFips.substring(2));
        this.displayFips = sb.toString();

        // Create the display string for the county name list.
        sb = new StringBuilder(state).append(",").append(this.countyName);
        this.displayCountyName = sb.toString();
    }

    public String getCountyName() {
        return countyName;
    }

    public void setCountyName(String countyName) {
        this.countyName = countyName;
    }

    public String getCountyFips() {
        return countyFips;
    }

    public void setCountyFips(String countyFips) {
        this.countyFips = countyFips;
    }

    public Long getGid() {
        return gid;
    }

    public void setGid(Long gid) {
        this.gid = gid;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getDisplayFips() {
        return displayFips;
    }

    public void setDisplayFips(String displayFips) {
        this.displayFips = displayFips;
    }

    public String getDisplayCountyName() {
        return displayCountyName;
    }

    public void setDisplayCountyName(String displayCountyName) {
        this.displayCountyName = displayCountyName;
    }

    @Override
    public int compareTo(FFMPCounty obj) {

        if (obj != null) {

            if (FFMPCounties.getSortBy() == CountySort.NAME) {
                return this.countyName.compareTo(obj.getCountyName());
            } else if (FFMPCounties.getSortBy() == CountySort.ID) {
                return this.countyFips.compareTo(obj.getCountyFips());
            } else if (FFMPCounties.getSortBy() == CountySort.DISPLAY_NAME) {
                return this.displayCountyName.compareTo(obj.getDisplayCountyName());
            }
        }

        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return countyName;
    }
}
