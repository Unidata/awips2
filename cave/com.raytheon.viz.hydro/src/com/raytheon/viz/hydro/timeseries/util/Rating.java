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
package com.raytheon.viz.hydro.timeseries.util;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Object holds the rating values for the Time Series Viewer.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 8, 2008	1194		mpduff	Initial creation
 * 
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class Rating {
    
    private String QUERY_SQL = "select lid,stage,discharge from rating where lid =':lid'";
    
    private String lid = null;
    private ArrayList<Double> stage = new ArrayList<Double>();
    private ArrayList<Double> discharge = new ArrayList<Double>();
    
    public Rating (String lid) {
        this.lid = lid;
        populate(lid);
    }
    
    private void populate(String lid) {

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(QUERY_SQL.replace(":lid", lid), HydroConstants.IHFS, QueryLanguage.SQL);
            if (results != null) {
                for (int i = 0; i < results.size(); i++) {
                    Object[] sa = results.get(i);
                    if (((sa[1] != null) || (sa[1] != "")) && ((sa[2] != null) || (sa[2] != ""))) {
                        //TODO check these types
                        addStage((Double)sa[1]);
                        addDischarge((Double)sa[2]);
                    }
                }
            }
        } catch (VizException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }
    
    /**
     * @param lid the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }
    
    /**
     * @return the stage
     */
    public ArrayList<Double> getStage() {
        return stage;
    }
    
    /**
     * @param stage the stage to set
     */
    public void setStage(ArrayList<Double> stage) {
        this.stage = stage;
    }
    
    /**
     * @return the discharge
     */
    public ArrayList<Double> getDischarge() {
        return discharge;
    }
    
    /**
     * @param discharge the discharge to set
     */
    public void setDischarge(ArrayList<Double> discharge) {
        this.discharge = discharge;
    }
    
    /**
     * Add a discharge value
     * 
     * @param discharge
     */
    public void addDischarge(double discharge) {
        this.discharge.add(discharge);
    }
    
    /**
     * Add a stage value
     * 
     * @param stage
     */
    public void addStage(double stage) {
        this.stage.add(stage);
    }
    
    /**
     * Get the corresponding discharge for the stage value
     * 
     * @param stage
     * @return the discharge corresponding to the stage value
     */
    public double getDischargeForStage(double stage) {
        for (int i = 0; i < this.stage.size(); i++) {
            if (this.stage.get(i) == stage) {
                return discharge.get(i);
            }
        }
        return HydroConstants.MISSING_VALUE;
    }

    /**
     * Get the corresponding stage for the discharge value
     * 
     * @param discharge
     * @return the stage corresponding to the discharge
     */
    public double getStageForDischarge(double discharge) {
        for (int i = 0; i < this.discharge.size(); i++) {
            if (this.discharge.get(i) == discharge) {
                return stage.get(i);
            }
        }
        return HydroConstants.MISSING_VALUE;
    }
}
