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

/**
 * This class contains the Publications data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2008 1782       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class PublicationsData extends HydroDBData implements IHydroDBData {

    /**
     * Location Identifier.
     */
    private String lid;

    /**
     * Beginning Date.
     */
    private Date begin;

    /**
     * Publication.
     */
    private String pub;

    /**
     * Ending Date.
     */
    private Date end;

    /**
     * Formats the date for the DB.
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor.
     */
    public PublicationsData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call.
     * 
     * @param data
     *            The raw data from the database.
     */
    public PublicationsData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setLid(getDBValue("lid", data, dataMap, ""));
        setBegin(getDBValue("pbegin", data, dataMap, (Date) null));
        setPub(getDBValue("ppub", data, dataMap, ""));
        setEnd(getDBValue("pend", data, dataMap, (Date) null));
    }

    /**
     * Method to format the date.
     */
    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Getter
     * 
     * @return lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * Setter
     * 
     * @param lid
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * Getter
     * 
     * @return begin
     */
    public Date getBegin() {
        return begin;
    }

    /**
     * Setter
     * 
     * @param begin
     */
    public void setBegin(Date begin) {
        this.begin = begin;
    }

    /**
     * Getter
     * 
     * @return pub
     */
    public String getPub() {
        return pub;
    }

    /**
     * Setter
     * 
     * @param pub
     */
    public void setPub(String pub) {
        this.pub = pub;
    }

    /**
     * Getter
     * 
     * @return end
     */
    public Date getEnd() {
        return end;
    }

    /**
     * Setter
     * 
     * @param end
     */
    public void setEnd(Date end) {
        this.end = end;
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid=%s AND pbegin=%s AND ppub=%s";
        return String.format(pkString, getDBString(lid), getDBString(begin,
                dateFormat), getDBString(pub));
    }

    @Override
    public String getSelectStatement() {
        return "SELECT lid, pbegin, ppub, pend FROM pub";
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid
                + "' ORDER BY pbegin DESC";
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO pub ( lid, pbegin, ppub, pend ) VALUES ( %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(begin,
                dateFormat), getDBString(pub), getDBString(end, dateFormat));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE pub SET lid=%s, pbegin=%s, ppub=%s, pend=%s WHERE %s";

        rval = String.format(rval, getDBString(lid), getDBString(begin,
                dateFormat), getDBString(pub), getDBString(end, dateFormat),
                getPKStatement());

        return rval;
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM pub WHERE %s", getPKStatement());
    }

}
