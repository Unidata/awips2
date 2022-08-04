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
package com.raytheon.viz.hydro.questionabledata;

import java.text.SimpleDateFormat;
import java.util.TimeZone;

import com.raytheon.viz.hydrocommon.data.PhysicalElementData;

/**
 * this class contains the Questionable/Bad data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Oct 22, 2008				askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class QuestionableData extends PhysicalElementData {
    // private SimpleDateFormat dateFormat = new SimpleDateFormat("MM-dd
    // HH:mm");

    private SimpleDateFormat obsFormat = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm");

    private SimpleDateFormat timeFormat = new SimpleDateFormat("MM-dd HH:mm");

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public QuestionableData(Object[] data) {
        super(data);
        timeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        obsFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    @Override
    public String toString() {
        return String
                .format(
                        "%-8s %-20.20s %2s %4s  %2s  %1s %8.8s  %16.16s   %s   %1s   %1.1s   %10s  %11.11s  %11.11s",
                        lid, name, pe, dur, ts, extremum, value, obsFormat
                                .format(obstime), (revision == 1) ? "T" : "F",
                        shefQualCode, getQualityCodeSymbol(), productID,
                        (productTime != null) ? timeFormat.format(productTime)
                                : "", (postingTime != null) ? timeFormat
                                .format(postingTime) : "");
    }
}
