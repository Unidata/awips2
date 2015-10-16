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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

/**
 * This class sets the Basin Trend Graph's boundaries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2015 4756       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class BasinTrendGraphBounds {

    /** name of the graph **/
    private String timeDurName;
    /** hours, (X Axis) **/
    private int hours;
    /** number of y Coordinate values **/
    private int yCoordValues;
    
    public BasinTrendGraphBounds() {
        
    }
    
    public String getTimeDurName()
    {
        return timeDurName;
    }
    
    public int getHours()
    {
        return hours;
    }
    
    public int getYCoordValues()
    {
        return yCoordValues;
    }

    public void setYCoordValues(int yCoordValues) {
        this.yCoordValues = yCoordValues;
    }

    public void setTimeDurName(String timeDurName) {
        this.timeDurName = timeDurName;
    }

    public void setHours(int hours) {
        this.hours = hours;
    }
    
    /**
     * Gets the label skipping for the Y axis of the Basin trend Graph
     * @return int
     */
    public int getSkipValue() {

        int skip = 0;

        if (yCoordValues > 0) {

            if (yCoordValues >= 50 && yCoordValues < 100) {
                skip = 2;
            } else if (yCoordValues >= 100 && yCoordValues < 200) {
                skip = 5;
            } else if (yCoordValues >= 200 && yCoordValues < 300) {
                skip = 10;
            } else if (yCoordValues >= 300 && yCoordValues < 500) {
                skip = 20;
            } else if (yCoordValues >= 500) {
                skip = 50;
            }
        }

        return skip;
    }
    
    
}
