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
 * this class contains the Flood data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 11, 2008				askripsky	Initial creation
 * Jan 7, 2008  1802        askripsky   Changed to extend HydroDBData
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class FloodData extends HydroDBData {

    /**
     * Lid that the data refers to
     */
    private String lid;

    /**
     * Stage.
     */
    private double stage;

    /**
     * Damage.
     */
    private String damage;

    /**
     * Display statement.
     */
    private String displayStatement;

    /**
     * Constructor
     */
    public FloodData() {
        lid = "";
        stage = 0;
        damage = "";
        displayStatement = "";
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public FloodData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setStage(getDBValue("stage", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setDisplayStatement(getDBValue("dispstmt", data, dataMap, ""));
        setDamage(getDBValue("damage", data, dataMap, ""));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public double getStage() {
        return stage;
    }

    public void setStage(double stage) {
        this.stage = stage;
    }

    public String getDamage() {
        return damage;
    }

    public void setDamage(String damage) {
        this.damage = damage;
    }

    public String getDisplayStatement() {
        return displayStatement;
    }

    public void setDisplayStatement(String displayStatement) {
        this.displayStatement = displayStatement;
    }

    @Override
    public String toString() {
        return String.format("%-8s       %-60s", String.format("%8.2f", stage),
                displayStatement);
    }
}
