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
 * this class contains the Adjust Factor data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 15, 2008	1787    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class DataAdjustFactorData extends HydroDBData implements IHydroDBData {

    /**
     * lid - Location/Station ID
     */
    private String lid;

    /**
     * Physical Element
     */
    private String pe;

    /**
     * Duration
     */
    private int duration;

    /**
     * Type Source
     */
    private String typeSource;

    /**
     * Extremum
     */
    private String extremum;

    /**
     * Divisor
     */
    private double divisor;

    /**
     * Base
     */
    private double base;

    /**
     * Multiplier
     */
    private double multiplier;

    /**
     * Adder
     */
    private double adder;

    /**
     * Constructor
     */
    public DataAdjustFactorData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public DataAdjustFactorData(QueryResultRow data,
            Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setPe(getDBValue("pe", data, dataMap, ""));
        setDuration(getDBValue("dur", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setTypeSource(getDBValue("ts", data, dataMap, ""));
        setExtremum(getDBValue("extremum", data, dataMap, ""));
        setDivisor(getDBValue("divisor", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setBase(getDBValue("base", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setMultiplier(getDBValue("multiplier", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setAdder(getDBValue("adder", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public String getTypeSource() {
        return typeSource;
    }

    public void setTypeSource(String typeSource) {
        this.typeSource = typeSource;
    }

    public String getExtremum() {
        return extremum;
    }

    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    /**
     * @return the divisor
     */
    public double getDivisor() {
        return divisor;
    }

    /**
     * @param divisor
     *            the divisor to set
     */
    public void setDivisor(double divisor) {
        this.divisor = divisor;
    }

    /**
     * @return the base
     */
    public double getBase() {
        return base;
    }

    /**
     * @param base
     *            the base to set
     */
    public void setBase(double base) {
        this.base = base;
    }

    /**
     * @return the multiplier
     */
    public double getMultiplier() {
        return multiplier;
    }

    /**
     * @param multiplier
     *            the multiplier to set
     */
    public void setMultiplier(double multiplier) {
        this.multiplier = multiplier;
    }

    /**
     * @return the adder
     */
    public double getAdder() {
        return adder;
    }

    /**
     * @param adder
     *            the adder to set
     */
    public void setAdder(double adder) {
        this.adder = adder;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM adjustfactor WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, pe, dur, ts, extremum, divisor, base, multiplier, adder";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM adjustfactor");
        rval.append(" WHERE ");
        rval.append(getPKStatement());

        return rval.toString();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, pe, dur, ts, extremum, divisor, base, multiplier, adder";

        String rval = "INSERT INTO adjustfactor ( " + columns
                + " ) VALUES ( '%s', '%s', %s, '%s', '%s', %s, %s, %s, %s )";

        rval = String.format(rval, lid, pe, getDBString(duration), typeSource,
                extremum, getDBString(divisor), getDBString(base),
                getDBString(multiplier), getDBString(adder));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid='" + getLid() + "' AND pe='" + getPe() + "' AND dur="
                + getDBString(getDuration()) + " AND ts='" + getTypeSource()
                + "' AND extremum='" + getExtremum() + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, pe, dur, ts, extremum, divisor, base, multiplier, adder";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM adjustfactor");
        rval.append(" ORDER BY lid, pe, dur, ts, extremum");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE adjustfactor SET lid='%s', pe='%s', dur=%s, ts='%s', "
                + "extremum='%s', divisor=%s, base=%s, multiplier=%s, adder=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, lid, pe, getDBString(duration), typeSource,
                extremum, getDBString(divisor), getDBString(base),
                getDBString(multiplier), getDBString(adder), getPKStatement());

        return rval;
    }
}
