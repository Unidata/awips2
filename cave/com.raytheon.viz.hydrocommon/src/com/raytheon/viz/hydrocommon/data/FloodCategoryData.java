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
 * this class contains the Flood Category data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 11, 2008				askripsky	Initial creation
 * March 3, 2011            JingtaoD    modification - need to display as blank if stored 
 *                                      in database as null. Not to display 0.0
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class FloodCategoryData {

    /**
     * Lid that the data refers to
     */
    private String lid;

    /**
     * Major Stage.
     */
    private double majorStage;

    /**
     * Moderate Stage.
     */
    private double moderateStage;

    /**
     * Minor Stage.
     */
    private double minorStage;

    /**
     * Major Discharge.
     */
    private double majorDischarge;

    /**
     * Moderate Discharge.
     */
    private double moderateDischarge;

    /**
     * Minor Discharge.
     */
    private double minorDischarge;
    
    /**
     * constant for missing value
     */
    private static final double missingVal = -9999.0;

    /**
     * Constructor
     */
    public FloodCategoryData() {
        lid = ""; 
        /* initialize as missing value because need to display as blank in the GUI if stored as null in database */
        majorStage = missingVal;
        moderateStage = missingVal;
        minorStage = missingVal;
        majorDischarge = missingVal;
        moderateDischarge = missingVal;
        minorDischarge = missingVal;
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public FloodCategoryData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid((String) data.getColumn(dataMap.get("lid")));
        
        if (data.getColumn(dataMap.get("major_stage")) != null) {
            setMajorStage((Double) data.getColumn(dataMap.get("major_stage")));
        }
        else
        	setMajorStage((Double) missingVal);
        
        if (data.getColumn(dataMap.get("moderate_stage")) != null) {
            setModerateStage((Double) data.getColumn(dataMap
                    .get("moderate_stage")));
        }
        else
        	setModerateStage((Double) missingVal);
        
        if (data.getColumn(dataMap.get("minor_stage")) != null) {
            setMinorStage((Double) data.getColumn(dataMap.get("minor_stage")));
        }
        else
        	setMinorStage((Double) missingVal);
        
        if (data.getColumn(dataMap.get("major_flow")) != null) {
            setMajorDischarge((Double) data
                    .getColumn(dataMap.get("major_flow")));
        }
        else
        	setMajorDischarge((Double) missingVal);
        	
         if (data.getColumn(dataMap.get("moderate_flow")) != null) {
            setModerateDischarge((Double) data.getColumn(dataMap
                    .get("moderate_flow")));            
        }
         else
         	setModerateDischarge((Double) missingVal);
       
        if (data.getColumn(dataMap.get("minor_flow")) != null) {
            setMinorDischarge((Double) data
                    .getColumn(dataMap.get("minor_flow")));
        }
        else
        	setMinorDischarge((Double) missingVal);
       
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public double getMajorStage() {
        return majorStage;
    }

    public void setMajorStage(double majorStage) {
        this.majorStage = majorStage;
    }

    public double getModerateStage() {
        return moderateStage;
    }

    public void setModerateStage(double moderateStage) {
        this.moderateStage = moderateStage;
    }

    public double getMinorStage() {
        return minorStage;
    }

    public void setMinorStage(double minorStage) {
        this.minorStage = minorStage;
    }

    public double getMajorDischarge() {
        return majorDischarge;
    }

    public void setMajorDischarge(double majorDischarge) {
        this.majorDischarge = majorDischarge;
    }

    public double getModerateDischarge() {
        return moderateDischarge;
    }

    public void setModerateDischarge(double moderateDischarge) {
        this.moderateDischarge = moderateDischarge;
    }

    public double getMinorDischarge() {
        return minorDischarge;
    }

    public void setMinorDischarge(double minorDischarge) {
        this.minorDischarge = minorDischarge;
    }
}
