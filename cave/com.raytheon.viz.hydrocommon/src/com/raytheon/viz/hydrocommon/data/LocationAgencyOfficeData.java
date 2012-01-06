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

/**
 * this class contains the data for the view: locextagency
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 20, 2008	1697		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class LocationAgencyOfficeData extends AgencyOfficeData implements
        IHydroDBData {

    /**
     * Location
     */
    private String lid;

    /**
     * Default Constructor
     */
    public LocationAgencyOfficeData() {

    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public LocationAgencyOfficeData(QueryResultRow data,
            Map<String, Integer> dataMap) {
        super(data, dataMap);

        setLid(getDBValue("lid", data, dataMap, ""));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getInsertStatement() {
        String rval = "INSERT INTO locextagency (lid, agency_code, office) VALUES ('%s', '%s', '%s')";

        // Populate the SQL with the values for the current object
        rval = String.format(rval, lid, getAgencyCode(), getOffice());

        return rval;
    }

    @Override
    public String getConstrainedSelectStatement() {
        String rval = getSelectStatement();

        rval = rval + " WHERE lid='" + getLid() + "'";
        return rval;
    }

    @Override
    public String getDeleteStatement() {
        String deleteQuery = "DELETE FROM locextagency WHERE %s";

        return String.format(deleteQuery, getPKStatement());
    }

    @Override
    public String getExistsStatement() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getPKStatement() {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(getLid());
        rval.append("' and agency_code='");
        rval.append(getAgencyCode());
        rval.append("' and office='");
        rval.append(getOffice());
        rval.append("'");

        return rval.toString();
    }

    @Override
    public String getSelectStatement() {
        return "SELECT lid, agency_code, office from locextagency";
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE locextagency SET (lid, agency_code, office) VALUES ('%s', '%s', '%s')";

        // Populate the values
        rval = String.format(rval, getLid(), getAgencyCode(), getOffice(),
                getPKStatement());

        return rval.toString();
    }
}
