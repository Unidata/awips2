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

import org.eclipse.swt.graphics.Color;

/**
 * Class containing the state to be drawn on the display.
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
public class CrestDrawData
{
    /**
     * Stage in feet.
     */
    private double stage = 0.0;
    
    /**
     * Year.
     */
    private int year = 0;
    
    /**
     * System colors are used so color does not need to be disposed of.
     */
    private Color color;
    
    /**
     * Flag determining if the data should be drawn or not.
     */
    private boolean drawFlag = true;
    
    /**
     * Flag indicating if the data is selected.
     */
    private boolean isSelected = false;
    
    /**
     * Constructor.
     * @param stage Stage (in feet).
     * @param year Year.
     * @param color Color of the data on the display.
     */
    public CrestDrawData(double stage, int year, Color color)
    {
        this.stage = stage;
        this.year = year;
        this.color = color;
    }

    /**
     * Get the draw data flag.
     * @return True if the data needs to be drawn, false otherwise.
     */
    public boolean getDrawData()
    {
        return drawFlag;
    }

    /**
     * Set the draw data flag.
     * @param drawFlag True if the data needs to be drawn, false otherwise.
     */
    public void setDrawData(boolean drawFlag)
    {
        this.drawFlag = drawFlag;
    }

    /**
     * Get the stage (in feet).
     * @return The stage value.
     */
    public double getStage()
    {
        return stage;
    }

    /**
     * Get the year.
     * @return The year.
     */
    public int getYear()
    {
        return year;
    }

    /**
     * Get the color of the data.  Note the color is a system color so
     * it does not get disposed.
     * @return The data color.
     */
    public Color getColor()
    {
        return color;
    }

    /**
     * Check if the data is selected.
     * @return True if the data is selected, false if unselected.
     */
    public boolean isSelected()
    {
        return isSelected;
    }

    /**
     * Set the data selection.
     * @param isSelected True if the data is selected, false if unselected.
     */
    public void setSelected(boolean isSelected)
    {
        this.isSelected = isSelected;
    }   
}
