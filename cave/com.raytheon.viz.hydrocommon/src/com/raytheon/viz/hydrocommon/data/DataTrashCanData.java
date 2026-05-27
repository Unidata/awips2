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
package com.raytheon.viz.hydrocommon.data;

import java.text.SimpleDateFormat;
import java.util.TimeZone;

import com.raytheon.viz.hydrocommon.IGetSortType;

/**
 * this class contains the Rejected data.
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
public class DataTrashCanData extends RejectedData implements
        Comparable<DataTrashCanData> {

    private IGetSortType sortType;

    private SimpleDateFormat timeFormat = new SimpleDateFormat("MM/dd HH:mm");

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public DataTrashCanData(Object[] data, IGetSortType sortType) {
        super(data);
        this.sortType = sortType;

        timeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    @Override
    public String toString() {
        String prodTime = null;
    
        if (productTime == null) {
            prodTime = " ";
        } else {
            prodTime = timeFormat.format(productTime);
        }
        
        return String
                .format(
                        "%-8s %-20.20s  %2s  %4s %2s  %1s %9.2f   %11s   %11s   %1s  %1s  %1s %-8s %4s %11s %-10s %11s",
                        lid, name, pe, dur, ts, extremum, value, timeFormat
                                .format(validTime), timeFormat
                                .format(basisTime), revision == 1 ? "T" : "F",
                        shefQualCode, getQualityCodeSymbol(), userID,
                        rejectType.compareToIgnoreCase("A") == 0 ? "Auto"
                                : "Man ", timeFormat.format(postingTime),
                        productID, prodTime);
    }

    @Override
    public int compareTo(DataTrashCanData o) {
        int rval = 0;

        String sortCriteria = sortType.getSortType();

        // Sort: ORDER BY lid, pe, ts, validtime DESC
        if (sortCriteria.compareTo("Location") == 0) {
            rval = lid.compareToIgnoreCase(o.getLid());

            if (0 == rval) {
                rval = pe.compareToIgnoreCase(o.getPe());

                if (0 == rval) {
                    rval = ts.compareToIgnoreCase(o.getTs());

                    if (0 == rval) {
                        rval = validTime.compareTo(o.getValidTime()) * -1;
                    }
                }
            }
        } else { // Sort: ORDER BY validtime DESC, lid, pe, ts
            rval = validTime.compareTo(o.getValidTime()) * -1;

            if (0 == rval) {
                rval = lid.compareToIgnoreCase(o.getLid());

                if (0 == rval) {
                    rval = pe.compareToIgnoreCase(o.getPe());

                    if (0 == rval) {
                        rval = ts.compareToIgnoreCase(o.getTs());
                    }
                }
            }
        }

        return rval;
    }
}
