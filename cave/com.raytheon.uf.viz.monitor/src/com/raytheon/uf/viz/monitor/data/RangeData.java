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
package com.raytheon.uf.viz.monitor.data;

/**
 * Class to hold range data with extra convenience methods.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class RangeData
{
    /**
     * Minimum value.
     */
    private int min = 0;
    
    /**
     * Maximum value.
     */
    private int max = 0;
    
    /**
     * Constructor.
     * @param min Minimum value.
     * @param max Maximum value.
     */
    public RangeData(int min, int max)
    {
        this.min = min;
        this.max = max;
    }
    
    /**
     * Get the minimum value.
     * @return The minimum value.
     */
    public int getMin()
    {
        return min;
    }
    
    /**
     * Get the maximum value.
     * @return The maximum value.
     */
    public int getMax()
    {
        return max;
    }
    
    /**
     * Check of the value passed in is in the range.  If the value is less
     * than the minimum a -1 is passed back.  If the value is in the range
     * then a 0 is passed back.  If the value is greater than the maximum
     * then 1 is passed back.
     * @param val Value to check
     * @return -1, 0, or 1
     */
    public int inRange(int val)
    {
        if (val < min)
        {
            return -1;
        }
        else if (val > max)
        {
            return 1;
        }
        
        return 0;
    }
}
