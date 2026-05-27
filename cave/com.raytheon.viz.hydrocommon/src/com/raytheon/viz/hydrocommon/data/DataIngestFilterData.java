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
 * this class contains the Data Ingest Filter data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Dec 12, 2008 1787        askripsky   Initial creation
 * Aug 07, 2015 4500        rjpeter     Fix type case.
 * JUN 29, 2016 19149       amoore      Fix PK statement based on table joining.
 * Jan 03, 2018  6806       mduff       Moved string formatting to this data object for caching and optimizations.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class DataIngestFilterData extends HydroDBData implements IHydroDBData {

    /**
     * lid - Location/Station ID
     */
    private String lid;

    /**
     * WFO.
     */
    private String wfo;

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
     * TS Rank
     */
    private int tsRank;

    /**
     * ingest
     */
    private String ingest;

    /**
     * ofs_input - OFS Input
     */
    private String ofsInput;

    /**
     * stg2_input - MPE Input
     */
    private String stg2Input;

    private String whereClause = "";

    private String formattedString;

    /**
     * Constructor
     */
    public DataIngestFilterData() {

    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public DataIngestFilterData(QueryResultRow data,
            Map<String, Integer> dataMap) {
        setLid(getDBValue("ingestfilter.lid", data, dataMap, ""));
        setWfo(getDBValue("wfo", data, dataMap, ""));
        setPe(getDBValue("pe", data, dataMap, ""));
        setDuration(getDBValue("dur", data, dataMap,
                (short) HydroConstants.MISSING_VALUE).intValue());
        setTypeSource(getDBValue("ts", data, dataMap, ""));
        setExtremum(getDBValue("extremum", data, dataMap, ""));
        setTsRank(getDBValue("ts_rank", data, dataMap,
                (short) HydroConstants.MISSING_VALUE).intValue());
        setIngest(getDBValue("ingest", data, dataMap, ""));
        setOfsInput(getDBValue("ofs_input", data, dataMap, ""));
        setStg2Input(getDBValue("stg2_input", data, dataMap, ""));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
        formattedString = null;
    }

    public String getWfo() {
        return wfo;
    }

    public void setWfo(String wfo) {
        this.wfo = wfo;
        formattedString = null;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
        formattedString = null;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
        formattedString = null;
    }

    public String getTypeSource() {
        return typeSource;
    }

    public void setTypeSource(String typeSource) {
        this.typeSource = typeSource;
        formattedString = null;
    }

    public String getExtremum() {
        return extremum;
    }

    public void setExtremum(String extremum) {
        this.extremum = extremum;
        formattedString = null;
    }

    public int getTsRank() {
        return tsRank;
    }

    public void setTsRank(int tsRank) {
        this.tsRank = tsRank;
        formattedString = null;
    }

    public String getIngest() {
        return ingest;
    }

    public void setIngest(String ingest) {
        this.ingest = ingest;
        formattedString = null;
    }

    public String getOfsInput() {
        return ofsInput;
    }

    public void setOfsInput(String ofsInput) {
        this.ofsInput = ofsInput;
        formattedString = null;
    }

    public String getStg2Input() {
        return stg2Input;
    }

    public void setStg2Input(String stg2Input) {
        this.stg2Input = stg2Input;
        formattedString = null;
    }

    public String getWhereClause() {
        return whereClause;
    }

    public void setWhereClause(String whereClause) {
        this.whereClause = whereClause;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + whereClause
                + " ORDER BY lid, wfo, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, stg2_input";
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM ingestfilter WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, stg2_input";

        String rval = "INSERT INTO ingestfilter ( " + columns
                + " ) VALUES ( '%s', '%s', %s, '%s', '%s', %s, '%s', '%s', '%s' )";

        rval = String.format(rval, lid, pe, getDBString(duration), typeSource,
                extremum, getDBString(tsRank), ingest, ofsInput, stg2Input);

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "ingestfilter.lid='" + getLid() + "' AND pe='" + getPe()
                + "' AND dur=" + getDBString(getDuration()) + " AND ts='"
                + getTypeSource() + "' AND extremum='" + getExtremum() + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuilder rval = new StringBuilder();

        rval.append("SELECT ");
        rval.append(
                "ingestfilter.lid, wfo, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, stg2_input");
        rval.append(
                " FROM ingestfilter LEFT JOIN location ON ingestfilter.lid = location.lid");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE ingestfilter SET lid='%s', pe='%s', dur=%s, ts='%s', "
                + "extremum='%s', ts_rank=%s, ingest='%s', ofs_input='%s', stg2_input='%s' WHERE %s";

        // Populate the values
        rval = String.format(rval, lid, pe, getDBString(duration), typeSource,
                extremum, getDBString(tsRank), ingest, ofsInput, stg2Input,
                getPKStatement());

        return rval;
    }

    /**
     * Returns the display string for the whole db record.
     * 
     * @param currData
     *            The record to display.
     * @return The display string for the data.
     */
    public String getIngestFilterString() {
        if (formattedString == null || formattedString.isEmpty()) {
            String dataFormat = "%-9S %-4S %-4S %-6S %-7S %-6S %-7S %-5S %-5S %-5S";
            formattedString = String.format(dataFormat, lid, wfo, pe,
                    getDisplayString(duration), typeSource, extremum,
                    getDisplayString(tsRank), ingest, ofsInput, stg2Input);
        }

        return formattedString;
    }

    /**
     * Returns the string corresponding to the DB value. Takes the MISSING_VALUE
     * into account.
     * 
     * @param val
     *            The int to get a display string for.
     * @return The corresponding string or "" if the value is MISSING_VALUE
     */
    private String getDisplayString(int val) {
        String temp = (val != HydroConstants.MISSING_VALUE)
                ? Integer.toString(val) : "";

        return temp;
    }
}
