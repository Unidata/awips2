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

import java.util.ArrayList;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2008            mpduff     Initial creation
 * Sep 09, 2009 2259       mpduff     Refactored to HydroCommon
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class Rating {
    private String lid = null;

    private ArrayList<Double> stage = new ArrayList<Double>();

    private ArrayList<Double> discharge = new ArrayList<Double>();

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
     * @return the stage
     */
    public ArrayList<Double> getStage() {
        return stage;
    }

    /**
     * @param stage
     *            the stage to set
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
     * @param discharge
     *            the discharge to set
     */
    public void setDischarge(ArrayList<Double> discharge) {
        this.discharge = discharge;
    }

    /**
     * Add discharge data to the list.
     * @param discharge
     */
    public void addDischarge(double discharge) {
        this.discharge.add(discharge);
    }
    
    /**
     * Add stage to the data list.
     * @param stage
     */
    public void addStage(double stage) {
        this.stage.add(stage);
    }
}
