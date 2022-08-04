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
 * This class contains the dam type data.
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

public class DamTypeData extends HydroDBData implements IHydroDBData {

    /**
     * Type of dam/reservoir.
     */
    private String type;

    /**
     * Constructor.
     */
    public DamTypeData() {
        type = "";
    }

    /**
     * Constructor.
     * 
     * @param data
     *            Query result data.
     * @param dataMap
     *            Row column to Index map.
     */
    public DamTypeData(QueryResultRow data, Map<String, Integer> dataMap) {
        setType(getDBValue("type", data, dataMap, ""));
    }

    /**
     * Get the type.
     * 
     * @return The type.
     */
    public String getType() {
        return type;
    }

    /**
     * Set the type.
     * 
     * @param type
     *            The type.
     */
    public void setType(String type) {
        this.type = type;
    }

    @Override
    public String getPKStatement() {
        String pkString = "type=%s";
        return String.format(pkString, getDBString(type));
    }

    @Override
    public String getSelectStatement() {
        return "SELECT type FROM damtypes order by type";
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
        String rval = "INSERT INTO damtypes ( type ) VALUES ( %s )";

        rval = String.format(rval, getDBString(type));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE damtypes SET type=%s WHERE %s";

        rval = String.format(rval, getDBString(type), getPKStatement());

        return rval;
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM damtypes WHERE %s", getPKStatement());
    }

}
