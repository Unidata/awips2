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
package com.raytheon.viz.hydrocommon.cresthistory;

import java.util.ArrayList;
import java.util.Collections;

/**
 * This class contains the collection of crest data and all of the level information
 * for the crest data: Starting Year, Levels (Major, Moderate, Minor, and Action), how to
 * sort the data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 3, 2008				lvenable	Initial creation
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class CrestHistoryData implements ICrestDataSortType
{
    /**
     * Array of crest data.
     */
    private ArrayList<CrestData> crestDataArray = null;
    
    /**
     * Starting year.
     */
    private int startingYear = 0;
    
    /**
     * Ending year.
     */
    private int endingYear = 0;
    
    
    /**
     * Maximum stage level.
     */
    private double maxStageLevel = 0.0;
    
    /**
     * Minimum stage level.
     */
    private double minStageLevel = 0.0;
    
    /**
     * Major stage level.
     */
    private double majorLevel = 0.0;
    
    /**
     * Moderate stage level.
     */
    private double moderateLevel = 0.0;
    
    /**
     * Minor level.
     */
    private double minorLevel = 0.0;
    
    /**
     * Action level.
     */
    private double actionLevel = 0.0;
    
    /**
     * Sort by enumeration.
     */
    private enum sortBy {Stage, Flow, Date};
    
    /**
     * Sort by key.
     */
    private sortBy sortKey;
    
    /**
     * Constructor.
     * @param maxStageLvl Maximum stage level.
     * @param minStageLvl Minimum stage level.
     * @param majorLvl Major level.
     * @param moderateLvl Moderate level. 
     * @param minorLvl Minor level.
     * @param actionLvl Action level.
     * @param startingYear Starting year.
     */
    public CrestHistoryData()
    {
       // default sortKey
       crestDataArray = new ArrayList<CrestData>();
       sortKey = sortBy.Stage;
    }
    
    /**
     * Add crest data to the crest data array.
     * @param crestData Crest data.
     */
    public void addCrestData(CrestData crestData)
    {
        crestData.setSortCallback(this);
        crestDataArray.add(crestData);
    }

    /**
     * Get the crest data array.
     * @return Array of crest data.
     */
    public ArrayList<CrestData> getCrestDataArray()
    {
        return crestDataArray;
    }

    /**
     * Get major level value.
     * @return Major level value.
     */
    public double getMajorLevel()
    {
        return majorLevel;
    }
    
    /**
     * Set major level value.
     * 
     */
    public void setMajorLevel(double majorLevel)
    {
        this.majorLevel = majorLevel;
    }

    /**
     * Get moderate level value.
     * @return Moderate level value.
     */
    public double getModerateLevel()
    {
        return moderateLevel;
    }
    
    /**
     * set moderate level value.
     */
    public void setModerateLevel(double moderateLevel)
    {
        this.moderateLevel = moderateLevel;
    }

    /**
     * Get minor level value.
     * @return Minor level value.
     */
    public double getMinorLevel()
    {
        return minorLevel;
    }
    
    /**
     * set minor level value.
     */
    public void setMinorLevel(double minorLevel)
    {
       this.minorLevel = minorLevel;
    }

    /**
     * Get action level value.
     * @return Action level value.
     */
    public double getActionLevel()
    {
        return actionLevel;
    }
    
    /**
     * Set action level value.
     */
    public void setActionLevel(double actionLevel)
    {
        this.actionLevel = actionLevel;
    }

    /**
     * Get maximum stage level value.
     * @return Maximum stage level value.
     */
    public double getMaxStageLevel()
    {
        return maxStageLevel;
    }
    
    /**
     * set maximum stage level value.
     */
    public void setMaxStageLevel(double maxStageLevel)
    {
        this.maxStageLevel = maxStageLevel;
    }

    /**
     * Get minimum stage level value.
     * @return Minimum stage level value.
     */
    public double getMinStageLevel()
    {
        return minStageLevel;
    }
    
    /**
     * set minimum stage level value.
     */
    public void setMinStageLevel(double minStageLevel)
    {
        this.minStageLevel = minStageLevel;
    }

    /**
     * Get the starting year.
     * @return The starting year.
     */
    public int getStartingYear()
    {
        return startingYear;
    }
    
    /**
     * Set the starting year.
     */
    public void setStartingYear(int startingYear)
    {
        this.startingYear = startingYear;
    }
    
    /**
     * Get the ending year.
     * @return The ending year.
     */
    public int getEndingYear()
    {
        return endingYear;
    }
    
    /**
     * Set the endingYear.
     */
    public void setEndingYear(int endingYear)
    {
        this.endingYear = endingYear;
    }

    /**
     * Get the sort type.
     */
    public String getSortType()
    {        
        return sortKey.name();
    }
    
    /**
     * Sort the crest data by stage value.
     */
    public void sortByStage()
    {
        sortKey = sortBy.Stage;
        sortCrestData();
    }
    
    /**
     * Sort the crest data by flow value.
     */
    public void sortByFlow()
    {
        sortKey = sortBy.Flow;
        sortCrestData();
    }
    
    /**
     * Sort the crest data by date value.
     */
    public void sortByDate()
    {
        sortKey = sortBy.Date;
        sortCrestData();
    }
    
    /**
     * Sort the crest data.
     */
    private void sortCrestData()
    {
        Collections.sort(crestDataArray);
    }
}
