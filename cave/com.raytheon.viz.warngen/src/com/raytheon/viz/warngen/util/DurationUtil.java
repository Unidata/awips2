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

import java.util.Calendar;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2009            bwoodle     Initial creation
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class DurationUtil {

    /**
     * Calculate the end time given a start time and a offset string in typical
     * time format ("40" == 40 minutes, "1:30" == 1 hour, 30 minutes)
     * 
     * @param cal
     * @param offset
     * @return
     */
    public static Calendar calcEndTime(Calendar cal, int minutes) {
        Calendar newCal = (Calendar) cal.clone();
        newCal.add(Calendar.MINUTE, minutes);
        return newCal;
    }

}
