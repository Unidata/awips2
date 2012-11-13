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
package com.raytheon.viz.warngen.util;

import java.util.Date;
import java.util.List;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2009            bwoodle     Initial creation
 * Nov 9, 2012  DR 15430   D. Friedman Support proper watch inclusion language
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class WeatherAdvisoryWatch implements Comparable<WeatherAdvisoryWatch> {
    
    public static class Portion {
        public String parentRegion;

        public List<String> partOfParentRegion;

        public String getParentRegion() {
            return parentRegion;
        }

        public void setParentRegion(String parentRegion) {
            this.parentRegion = parentRegion;
        }

        public List<String> getPartOfParentRegion() {
            return partOfParentRegion;
        }

        public void setPartOfParentRegion(List<String> partOfParentRegion) {
            this.partOfParentRegion = partOfParentRegion;
        }
    }

    /* TODO: NOTE: There is no site field.  We currently only process
     * WCNs for the site and not WOUs from the SPC.
     */
    
    private String phensig;
    
    private int eventId;

    private Date endTime;
    
    private List<Portion> portions;

    @Deprecated
    private String parentRegion;

    @Deprecated
    private List<String> partOfParentRegion;

    public String getPhensig() {
        return phensig;
    }

    public void setPhensig(String phensig) {
        this.phensig = phensig;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    @Deprecated
    public String getParentRegion() {
        return parentRegion;
    }

    @Deprecated
    public void setParentRegion(String parentRegion) {
        this.parentRegion = parentRegion;
    }

    @Deprecated
    public List<String> getPartOfParentRegion() {
        return partOfParentRegion;
    }

    @Deprecated
    public void setPartOfParentRegion(List<String> partOfParentRegion) {
        this.partOfParentRegion = partOfParentRegion;
    }
    
    @Override
    public boolean equals(Object obj) {        
        return obj instanceof WeatherAdvisoryWatch &&
            this.compareTo((WeatherAdvisoryWatch) obj) == 0;
    }

    public int compareTo(WeatherAdvisoryWatch waw) {
        if (this.phensig == null)
            return waw.phensig == null ? 0 : -1;
        else if (waw.phensig == null)
            return 1;
        else {
            int c = this.phensig.compareTo(waw.phensig);
            if (c == 0)
                return this.eventId - waw.eventId;
            else
                return c;
        }
    }

    public int getEventId() {
        return eventId;
    }

    public void setEventId(int eventId) {
        this.eventId = eventId;
    }

    public List<Portion> getPortions() {
        return portions;
    }

    public void setPortions(List<Portion> portions) {
        this.portions = portions;
    }
}
