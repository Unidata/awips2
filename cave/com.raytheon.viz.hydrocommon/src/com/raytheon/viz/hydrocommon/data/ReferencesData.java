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
 * This class contains the references data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20 Nov 2008              lvenable    Initial creation.
 * 12/19/2008   1782        grichard    Connected to IHFS DB.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ReferencesData extends HydroDBData implements IHydroDBData {
    /**
     * Location ID.
     */
    private String lid;

    /**
     * Contact name.
     */
    private String reference;

    /**
     * Constructor.
     */
    public ReferencesData() {
        lid = "";
        reference = "";
    }

    /**
     * Constructor.
     * 
     * @param data
     *            Query result data.
     * @param dataMap
     *            Row column to Index map.
     */
    public ReferencesData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, "ZZZZZ"));
        setReference(getDBValue("reference", data, dataMap, ""));
    }

    /**
     * Get the location ID.
     * 
     * @return The location ID.
     */
    public String getLid() {
        return lid;
    }

    /**
     * Set the location ID.
     * 
     * @param lid
     *            The location ID.
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * Get the reference.
     * 
     * @return The reference.
     */
    public String getReference() {
        return reference;
    }

    /**
     * Set the reference.
     * 
     * @param reference
     *            The reference.
     */
    public void setReference(String reference) {
        this.reference = reference;
    }

    /**
     * toString method that returns the reference data.
     * 
     * @return The reference data.
     */
    @Override
    public String toString() {
        return reference;
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid=%s AND reference=%s";
        return String
                .format(pkString, getDBString(lid), getDBString(reference));
    }

    @Override
    public String getSelectStatement() {
        return "SELECT lid, reference FROM refer";
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid
                + "' ORDER BY reference DESC";
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO refer ( lid, reference ) VALUES ( %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(reference));

        return rval;
    }

    @Override
    public String getUpdateStatement() {            	
    	String rval = "UPDATE refer SET lid=%s, reference=%s ";
    	rval = String.format(rval, getDBString(lid), getDBString(reference));
    	
        return rval;
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM refer WHERE %s", getPKStatement());
    }

}
