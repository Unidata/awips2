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
package com.raytheon.viz.aviation.climatedata;

/**
 * Class containing data for graphing the number observations for each year. 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2010            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ObsGraphData implements Comparable<ObsGraphData>
{
    /**
     * Year.
     */
    private int year = 0;
    
    /**
     * Number of observations.
     */
    private int numObs = 0;
    
    /**
     * Constructor.
     * @param year Year.
     * @param numObs Number of observations.
     */
    public ObsGraphData(int year, int numObs)
    {
        this.year = year;
        this.numObs = numObs;
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
     * Get the number of observations.
     * @return The number of observations.
     */
    public int getNumObs()
    {
        return numObs;
    }

    /**
     * compareTo method to sort by the year.
     */
    @Override
    public int compareTo(ObsGraphData obj)
    {
        if (year < obj.year)
        {
            return -1;
        }
        else if (year > obj.year)
        {
            return 1;
        }
        
        return 0;
    }
}
