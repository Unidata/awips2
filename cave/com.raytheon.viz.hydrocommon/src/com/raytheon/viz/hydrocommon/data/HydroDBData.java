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
import java.util.Date;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class is the base class for HydroView and HydroBase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 18, 2008	1697		askripsky	Initial creation
 * Dec 2, 2008  1744        askripsky   Added getDBString methods
 * Dec 17, 2008 1782        grichard    Added getDBString methods
 * Jan 03, 2013 15520       lbousaidi   Added getDBStringNoQuote method

 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class HydroDBData {
    /**
     * Checks if the value exists in the DB result and is not null. If not,
     * returns the defaultValue. Else, casts the results from the DB.
     * 
     * @param <T>
     * @param column
     * @param data
     * @param dataMap
     * @param defaultValue
     * @return
     */
    @SuppressWarnings("unchecked")
    protected <T> T getDBValue(String column, QueryResultRow data,
            Map<String, Integer> dataMap, T defaultValue) {
        T rval = null;

        if (dataMap.containsKey(column)
                && data.getColumn(dataMap.get(column)) != null) {
            rval = (T) data.getColumn(dataMap.get(column));
        } else {
            rval = defaultValue;
        }

        return rval;
    }

    /**
     * Returns "null" if the value is set to the missing_value constant, else
     * returns the value
     * 
     * @param val
     *            A double value to test to see if it is missing
     * @return
     */
    public String getDBString(double val) {
        return (Double.compare(val, Double
                .valueOf(HydroConstants.MISSING_VALUE)) != 0) ? Double
                .toString(val) : "null";
    }

    /**
     * Returns "null" if the value is set to the missing_value constant, else
     * returns the value
     * 
     * @param val
     *            A integer value to test to see if it is missing
     * @return
     */
    public String getDBString(int val) {
        return (val != HydroConstants.MISSING_VALUE) ? Integer.toString(val)
                : "null";
    }

    /**
     * Returns "null" if the value is set to null, else returns the value
     * 
     * @param date
     * @param sdf
     * @return
     */
    public String getDBString(Date date, SimpleDateFormat sdf) {
        return (date != null) ? "'" + sdf.format(date) + "'" : "null";
    }

    /**
     * Returns "null" if the value is set to null, else returns the value
     * 
     * @param str
     * @return
     */
    public String getDBString(String str) {
        return (str != null) ? "'" + str + "'" : "null";
    }
    
    /**
     * Returns "null" if the value is set to null, else returns the value
     * Doesn't add extra  single quotes around the string
     * @param str
     * @return
     */

    public String getDBStringNoQuote(String str) {
        return (str != null) ?  str  : "null";
    }

}
