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
 * this class contains the data for the view: agencyofficeunique
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
public class AgencyOfficeData extends HydroDBData implements IHydroDBData {

    /**
     * Agency code
     */
    private String agencyCode;

    /**
     * Office.
     */
    private String office;

    /**
     * Default Constructor
     */
    public AgencyOfficeData() {

    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public AgencyOfficeData(QueryResultRow data, Map<String, Integer> dataMap) {

        setAgencyCode(getDBValue("agency_code", data, dataMap, ""));
        setOffice(getDBValue("office", data, dataMap, ""));
    }

    public String getAgencyCode() {
        return agencyCode;
    }

    public void setAgencyCode(String agencyCode) {
        this.agencyCode = agencyCode;
    }

    public String getOffice() {
        return office;
    }

    public void setOffice(String office) {
        this.office = office;
    }

    @Override
    public String getConstrainedSelectStatement() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getDeleteStatement() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getExistsStatement() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getInsertStatement() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getPKStatement() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT agency_code, office from agencyofficeunique";
    }

    @Override
    public String getUpdateStatement() {
        // TODO Auto-generated method stub
        return null;
    }
}
