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
 * this class contains the Purge Product data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 17, 2008	1787		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class PurgeProductData extends HydroDBData implements IHydroDBData {

    /**
     * Product ID
     */
    private String productID;

    /**
     * Number of versions to keep
     */
    private int numberOfVersions;

    /**
     * Product Time
     */
    private Date productTime;

    /**
     * Posting Time
     */
    private Date postingTime;

    /**
     * Date format for the date/time values
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor
     */
    public PurgeProductData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public PurgeProductData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setProductID(getDBValue("product_id", data, dataMap, ""));
        setNumberOfVersions(getDBValue("num_versions", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setProductTime(getDBValue("producttime", data, dataMap, (Date) null));
        setPostingTime(getDBValue("postingtime", data, dataMap, (Date) null));
    }

    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * @return the productID
     */
    public String getProductID() {
        return productID;
    }

    /**
     * @param productID
     *            the productID to set
     */
    public void setProductID(String productID) {
        this.productID = productID;
    }

    /**
     * @return the numberOfVersions
     */
    public int getNumberOfVersions() {
        return numberOfVersions;
    }

    /**
     * @param numberOfVersions
     *            the numberOfVersions to set
     */
    public void setNumberOfVersions(int numberOfVersions) {
        this.numberOfVersions = numberOfVersions;
    }

    /**
     * @return the productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * @param productTime
     *            the productTime to set
     */
    public void setProductTime(Date productTime) {
        this.productTime = productTime;
    }

    /**
     * @return the postingTime
     */
    public Date getPostingTime() {
        return postingTime;
    }

    /**
     * @param postingTime
     *            the postingTime to set
     */
    public void setPostingTime(Date postingTime) {
        this.postingTime = postingTime;
    }

    /**
     * @return the dateFormat
     */
    public SimpleDateFormat getDateFormat() {
        return dateFormat;
    }

    /**
     * @param dateFormat
     *            the dateFormat to set
     */
    public void setDateFormat(SimpleDateFormat dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO purgeproduct ( product_id, num_versions, producttime, postingtime ) VALUES ( %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(productID),
                getDBString(numberOfVersions), getDBString(productTime,
                        dateFormat), getDBString(postingTime, dateFormat));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        // First used in the Data Purge Dlg which doesn't touch the
        // posting/product
        // times
        String rval = "UPDATE purgeproduct SET product_id=%s, num_versions=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(productID),
                getDBString(numberOfVersions), getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT product_id, num_versions, producttime, postingtime FROM purgeproduct ORDER BY product_id";
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM purgeproduct WHERE " + getPKStatement();
    }

    @Override
    public String getPKStatement() {
        // There is no PK defined for the purgeproduct table in Postgres
        return "product_id=" + getDBString(productID);
    }

    @Override
    public String getExistsStatement() {
        return "SELECT product_id FROM purgeproduct WHERE " + getPKStatement();
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }
}
