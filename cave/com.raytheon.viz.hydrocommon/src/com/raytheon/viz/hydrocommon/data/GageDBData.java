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
 * This class contains the data for the Gage table.
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
public class GageDBData extends HydroDBData implements IHydroDBData {

    /**
     * lid
     */
    private String lid;

    /**
     * gbegin date
     */
    private Date beginDate;

    /**
     * type
     */
    private String type;

    /**
     * gend date
     */
    private Date endDate;

    /**
     * remark
     */
    private String remark;

    /**
     * maint
     */
    private String maintainingAgency;

    /**
     * Owner
     */
    private String owner;

    /**
     * Date format
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor
     */
    public GageDBData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public GageDBData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setLid(getDBValue("lid", data, dataMap, ""));
        setBeginDate(getDBValue("gbegin", data, dataMap, (Date) null));
        setType(getDBValue("type", data, dataMap, ""));
        setEndDate(getDBValue("gend", data, dataMap, (Date) null));
        setRemark(getDBValue("remark", data, dataMap, ""));
        setMaintainingAgency(getDBValue("maint", data, dataMap, ""));
        setOwner(getDBValue("owner", data, dataMap, ""));
    }

    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * @return the owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * @param owner
     *            the owner to set
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the beginDate
     */
    public Date getBeginDate() {
        return beginDate;
    }

    /**
     * @param beginDate
     *            the beginDate to set
     */
    public void setBeginDate(Date beginDate) {
        this.beginDate = beginDate;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the endDate
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @param endDate
     *            the endDate to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * @return the remark
     */
    public String getRemark() {
        return remark;
    }

    /**
     * @param remark
     *            the remark to set
     */
    public void setRemark(String remark) {
        this.remark = remark;
    }

    /**
     * @return the maintainingAgency
     */
    public String getMaintainingAgency() {
        return maintainingAgency;
    }

    /**
     * @param maintainingAgency
     *            the maintainingAgency to set
     */
    public void setMaintainingAgency(String maintainingAgency) {
        this.maintainingAgency = maintainingAgency;
    }

    @Override
    public String getConstrainedSelectStatement() {
        String whereClause = " WHERE lid='" + lid
                + "' ORDER BY gend DESC, gbegin DESC";
        return getSelectStatement() + whereClause;
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM gage WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM gage WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, gbegin, type, gend, remark, maint, owner";

        String rval = "INSERT INTO gage ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(beginDate,
                dateFormat), getDBString(type),
                getDBString(endDate, dateFormat), getDBString(remark),
                getDBString(maintainingAgency), getDBString(owner));

        return rval;
    }

    @Override
    public String getPKStatement() {
        String pk = "lid=%s AND gbegin=%s AND type=%s";

        return String.format(pk, getDBString(lid), getDBString(beginDate,
                dateFormat), getDBString(type));
    }

    @Override
    public String getSelectStatement() {
        String columns = "lid, gbegin, type, gend, remark, maint, owner";

        String query = "SELECT " + columns + " FROM gage";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE gage SET lid=%s, gbegin=%s, type=%s, gend=%s, remark=%s, maint=%s, owner=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(beginDate,
                dateFormat), getDBString(type),
                getDBString(endDate, dateFormat), getDBString(remark),
                getDBString(maintainingAgency), getDBString(owner),
                getPKStatement());

        return rval;
    }
}
