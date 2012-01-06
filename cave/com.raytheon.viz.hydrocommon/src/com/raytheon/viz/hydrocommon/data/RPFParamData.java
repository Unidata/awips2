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

import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * this class contains the RiverPro General Parameters data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 18, 2008	1787		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class RPFParamData extends HydroDBData implements IHydroDBData {
    /**
     * obshrs - Hours to look back for obs data
     */
    private int observationHours = HydroConstants.MISSING_VALUE;

    /**
     * fcsthrs - Hours to look back for forecast data
     */
    private int forecastHours = HydroConstants.MISSING_VALUE;

    /**
     * missval - String to use for missing data
     */
    private String missingValue = "";

    /**
     * misscat - String to use for missing stage category
     */
    private String missingCategory = "";

    /**
     * misstim - String to use for missing Date/Time
     */
    private String missingTime = "";

    /**
     * rvsexphrs - Default hours before expiring RVS
     */
    private int rvsHours = HydroConstants.MISSING_VALUE;

    /**
     * flsexphrs - Default hours before expiring FLS
     */
    private int flsHours = HydroConstants.MISSING_VALUE;

    /**
     * flwexphrs - Default hours before expiring FLW
     */
    private int flwHours = HydroConstants.MISSING_VALUE;

    /**
     * Constructor
     */
    public RPFParamData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public RPFParamData(QueryResultRow data, Map<String, Integer> dataMap) {
        setObservationHours(getDBValue("obshrs", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setForecastHours(getDBValue("fcsthrs", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setMissingValue(getDBValue("missval", data, dataMap, ""));
        setMissingCategory(getDBValue("misscat", data, dataMap, ""));
        setMissingTime(getDBValue("misstim", data, dataMap, ""));
        setRvsHours(getDBValue("rvsexphrs", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setFlsHours(getDBValue("flsexphrs", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setFlwHours(getDBValue("flwexphrs", data, dataMap,
                HydroConstants.MISSING_VALUE));
    }

    /**
     * @return the observationHours
     */
    public int getObservationHours() {
        return observationHours;
    }

    /**
     * @param observationHours
     *            the observationHours to set
     */
    public void setObservationHours(int observationHours) {
        this.observationHours = observationHours;
    }

    /**
     * @return the forecastHours
     */
    public int getForecastHours() {
        return forecastHours;
    }

    /**
     * @param forecastHours
     *            the forecastHours to set
     */
    public void setForecastHours(int forecastHours) {
        this.forecastHours = forecastHours;
    }

    /**
     * @return the missingValue
     */
    public String getMissingValue() {
        return missingValue;
    }

    /**
     * @param missingValue
     *            the missingValue to set
     */
    public void setMissingValue(String missingValue) {
        this.missingValue = missingValue;
    }

    /**
     * @return the missingCategory
     */
    public String getMissingCategory() {
        return missingCategory;
    }

    /**
     * @param missingCategory
     *            the missingCategory to set
     */
    public void setMissingCategory(String missingCategory) {
        this.missingCategory = missingCategory;
    }

    /**
     * @return the missingTime
     */
    public String getMissingTime() {
        return missingTime;
    }

    /**
     * @param missingTime
     *            the missingTime to set
     */
    public void setMissingTime(String missingTime) {
        this.missingTime = missingTime;
    }

    /**
     * @return the rvsHours
     */
    public int getRvsHours() {
        return rvsHours;
    }

    /**
     * @param rvsHours
     *            the rvsHours to set
     */
    public void setRvsHours(int rvsHours) {
        this.rvsHours = rvsHours;
    }

    /**
     * @return the flsHours
     */
    public int getFlsHours() {
        return flsHours;
    }

    /**
     * @param flsHours
     *            the flsHours to set
     */
    public void setFlsHours(int flsHours) {
        this.flsHours = flsHours;
    }

    /**
     * @return the flwHours
     */
    public int getFlwHours() {
        return flwHours;
    }

    /**
     * @param flwHours
     *            the flwHours to set
     */
    public void setFlwHours(int flwHours) {
        this.flwHours = flwHours;
    }

    @Override
    public String getInsertStatement() {
        String columns = "obshrs, fcsthrs, missval, misscat, misstim, rvsexphrs, flsexphrs, flwexphrs";
        String rval = "INSERT INTO rpfparams ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(observationHours),
                getDBString(forecastHours), getDBString(missingValue),
                getDBString(missingCategory), getDBString(missingTime),
                getDBString(rvsHours), getDBString(flsHours),
                getDBString(flwHours));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE rpfparams SET obshrs=%s, fcsthrs=%s, missval=%s, misscat=%s, misstim=%s, rvsexphrs=%s, flsexphrs=%s, flwexphrs=%s";

        // Populate the values
        rval = String.format(rval, getDBString(observationHours),
                getDBString(forecastHours), getDBString(missingValue),
                getDBString(missingCategory), getDBString(missingTime),
                getDBString(rvsHours), getDBString(flsHours),
                getDBString(flwHours));

        return rval;
    }

    @Override
    public String getSelectStatement() {
        String columns = "obshrs, fcsthrs, missval, misscat, misstim, rvsexphrs, flsexphrs, flwexphrs";
        return "SELECT " + columns + " FROM rpfparams";
    }

    @Override
    public String getDeleteStatement() {
        return "";
    }

    @Override
    public String getPKStatement() {
        // Only one record should be in the table
        return "missval like '%'";
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = getSelectStatement() + " WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }
}
