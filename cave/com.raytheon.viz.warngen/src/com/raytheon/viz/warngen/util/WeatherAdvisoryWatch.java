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
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class WeatherAdvisoryWatch implements Comparable<WeatherAdvisoryWatch> {

    private String phensig;

    private Date endTime;

    private String parentRegion;

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

    public int compareTo(WeatherAdvisoryWatch waw) {
        return this.parentRegion.compareTo(waw.getParentRegion());
    }
}
