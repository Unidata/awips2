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
 * This class contains the data for the City.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 8, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class CityData extends HydroDBData implements IHydroDBData {

    /**
     * name
     */
    private String name;

    /**
     * State
     */
    private String state;

    /**
     * Latitude
     */
    private double latitude;

    /**
     * Longitude
     */
    private double longitude;

    /**
     * Display Precedence
     */
    private int displayPrecedence;

    /**
     * Population
     */
    private int population;

    /**
     * Constructor
     */
    public CityData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public CityData(QueryResultRow data, Map<String, Integer> dataMap) {
        setName(getDBValue("name", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setLatitude(getDBValue("lat", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setLongitude(getDBValue("lon", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setDisplayPrecedence(getDBValue("disp_precedence", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setPopulation(getDBValue("population", data, dataMap,
                HydroConstants.MISSING_VALUE));
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return the latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the displayPrecedence
     */
    public int getDisplayPrecedence() {
        return displayPrecedence;
    }

    /**
     * @param displayPrecedence
     *            the displayPrecedence to set
     */
    public void setDisplayPrecedence(int displayPrecedence) {
        this.displayPrecedence = displayPrecedence;
    }

    /**
     * @return the population
     */
    public int getPopulation() {
        return population;
    }

    /**
     * @param population
     *            the population to set
     */
    public void setPopulation(int population) {
        this.population = population;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM city WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT state FROM city WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "name, state, lat, lon, disp_precedence, population";

        String rval = "INSERT INTO city ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(name), getDBString(state),
                getDBString(latitude), getDBString(longitude),
                getDBString(displayPrecedence), getDBString(population));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "name=" + getDBString(name) + " AND state=" + getDBString(state);
    }

    @Override
    public String getSelectStatement() {
        String columns = "name, state, lat, lon, disp_precedence, population";

        String query = "SELECT " + columns + " FROM city ORDER BY state, name";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE city SET name=%s, state=%s, lat=%s, lon=%s, disp_precedence=%s, population=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(name), getDBString(state),
                getDBString(latitude), getDBString(longitude),
                getDBString(displayPrecedence), getDBString(population),
                getPKStatement());

        return rval;
    }
}
