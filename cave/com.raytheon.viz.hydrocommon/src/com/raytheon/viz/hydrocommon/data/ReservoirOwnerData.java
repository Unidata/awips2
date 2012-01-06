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
 * This class contains the reservoir owner data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2008 1782       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ReservoirOwnerData extends HydroDBData implements IHydroDBData {

    /**
     * Owner of dam/reservoir.
     */
    private String owner;

    /**
     * Constructor.
     */
    public ReservoirOwnerData() {
        owner = "";
    }

    /**
     * Constructor.
     * 
     * @param data
     *            Query result data.
     * @param dataMap
     *            Row column to Index map.
     */
    public ReservoirOwnerData(QueryResultRow data, Map<String, Integer> dataMap) {
        setOwner(getDBValue("owner", data, dataMap, ""));
    }

    /**
     * Get the owner.
     * 
     * @return The owner.
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Set the owner.
     * 
     * @param owner
     *            The owner.
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    @Override
    public String getPKStatement() {
        String pkString = "owner=%s";
        return String.format(pkString, getDBString(owner));
    }

    @Override
    public String getSelectStatement() {
        return "SELECT owner FROM resowner order by owner";
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO resowner ( owner ) VALUES ( %s )";

        rval = String.format(rval, getDBString(owner));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE resowner SET owner=%s WHERE %s";

        rval = String.format(rval, getDBString(owner), getPKStatement());

        return rval;
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM resowner WHERE %s", getPKStatement());
    }

}
