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

package com.raytheon.viz.hydro.flashfloodguidance;

/**
 * This class contains the bar data for the color legend.
 * 
 * The color name can be found in the rgb.txt file located
 * here:  build/static/common/cave/etc/colorfile
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class ColorLegendBarData
{
    /**
     * Duration.
     */
    private double duration;
    
    /**
     * Name of the color.
     */
    private String colorName;
    
    /**
     * Constructor.
     * @param duration Duration
     * @param colorName Color name.
     */
    public ColorLegendBarData(double duration, String colorName)
    {
        this.duration = duration;
        this.colorName = colorName;
    }
    
    /**
     * Get the duration.
     * @return The duration.
     */
    public double getDuration()
    {
        return duration;
    }
    
    /**
     * Get the color name.
     * @return the color name.
     */
    public String getColorName()
    {
        return colorName;
    }
}
