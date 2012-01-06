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
package com.raytheon.uf.viz.monitor.data;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * This class contains and manages the green, yellow, and red threat level
 * observation date-time lists needed to determine the overall threat level for
 * one monitoring area type (SAFESEAS or fog monitor) and one observation type
 * (image or point observation).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2009 1999       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ObTime {

    // List of observation times related to the threat level
    private List<Date> obTimeList = new ArrayList<Date>();

    // The threat level of this instance
    private ObConst.ThreatLevel threatLevel;

    /**
     * Public constructor
     * 
     * @param tl
     *            -- the threat level, typically RED, YELLOW, or GREEN
     */
    public ObTime(ObConst.ThreatLevel tl) {
        this.threatLevel = tl;
    }

    // Getter of observation time list related the threat levels
    public List<Date> getObTimeList() {
        return obTimeList;
    }

    // Prune the observation times two or more hours old
    public void pruneObTimeList(Date dropTime) {
        // Declare temporary list to avoid ConcurrentModificationException
        List<Date> myObTimeList = new ArrayList<Date>();
        for (Date d : obTimeList) {
            if (d.before(dropTime)) {
                // Add dates to age-out
                myObTimeList.add(d);
            }
        }
        // Use temporary list to age-out entries in obTimeList, since the list
        // used to iterate cannot be concurrently modified in the same loop
        for (Date d : myObTimeList) {
            obTimeList.remove(d);
        }
    }

    // Clear the list of observation times
    public void clearObTimeList() {
        obTimeList.clear();
    }

    // Store the date/time object
    public void storeDateTime(Date datetime) {
        obTimeList.add(datetime);
    }

    // Determine threat level
    public ObConst.ThreatLevel detThreatLevel() {
        pruneObTimeList(ObUtil.getDropTime());
        if (!obTimeList.isEmpty()) {
            return threatLevel;
        }
        return ObConst.ThreatLevel.GRAY;
    }

    // Determine whether observation time list is empty
    public boolean isObTimeListEmpty() {
        return obTimeList.isEmpty();
    }

}
