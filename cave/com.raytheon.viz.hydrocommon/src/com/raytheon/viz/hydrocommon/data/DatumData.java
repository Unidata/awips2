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
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * this class contains the Datum data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 05, 2008	1744		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class DatumData extends HydroDBData implements IHydroDBData {

    /**
     * Location
     */
    private String lid;

    /**
     * Date.
     */
    private Date date;

    /**
     * Elevation.
     */
    private double elevation;

    /**
     * Formats the date for the DB
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor
     */
    public DatumData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public DatumData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setLid(getDBValue("lid", data, dataMap, ""));
        setDate(getDBValue("ddate", data, dataMap, (Date) null));
        setElevation(getDBValue("elev", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
    }

    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public Date getDate() {
        return date;
    }

    public String getDateDBString() {
        return (date != null) ? "'" + dateFormat.format(date) + "'" : "null";
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public double getElevation() {
        return elevation;
    }

    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO datum ( lid, ddate, elev ) VALUES ( '%s', %s, %s )";

        rval = String.format(rval, getLid(), getDateDBString(),
                getDBString(elevation));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE datum SET elev='%s' WHERE %s";

        // Populate the values
        rval = String.format(rval, getElevation(), getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT lid, ddate, elev FROM datum";
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM datum WHERE %s", getPKStatement());
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid='%s' AND ddate=%s";
        return String.format(pkString, lid, getDateDBString());
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = getSelectStatement() + " WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid
                + "' ORDER BY ddate DESC";
    }
}
