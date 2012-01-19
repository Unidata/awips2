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
package com.raytheon.uf.common.monitor.scan.config;

/**
 * 
 * DMD display filter configuration data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2009  #3039      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class DmdDisplayFilterConfig
{
    /**
     * Track option.
     */
    private boolean track = false;
    
    /**
     * Overlap option.
     */
    private boolean overlap = false;
    
    /**
     * Upper value.
     */
    private double upperVal = Double.NaN;
    
    /**
     * Mid value.
     */
    private double midVal = Double.NaN;
    
    /**
     * Lower value.
     */
    private double lowerVal = Double.NaN;
    
    /**
     * Constructor.
     * @param track Track option.
     * @param overlap Overlap option.
     * @param upperVal Upper value.
     * @param midVal Mid value.
     * @param lowerVal Lower value.
     */
    public DmdDisplayFilterConfig(boolean track, boolean overlap,
              double upperVal, double midVal, double lowerVal)
    {
        this.track = track;
        this.overlap = overlap;
        this.upperVal = upperVal;
        this.midVal = midVal;
        this.lowerVal = lowerVal;
    }

    /**
     * Check the track option.
     * @return True if the track option is selected.
     */
    public boolean isTrack()
    {
        return track;
    }

    /**
     * Check the overlap option.
     * @return True if the overlap option is selected.
     */
    public boolean isOverlap()
    {
        return overlap;
    }

    /**
     * Get the upper value.
     * @return The upper value.
     */
    public double getUpperVal()
    {
        return upperVal;
    }

    /**
     * Get the mid value.
     * @return The mid value.
     */
    public double getMidVal()
    {
        return midVal;
    }

    /**
     * Get the lower value.
     * @return The lower value.
     */
    public double getLowerVal() 
    {
        return lowerVal;
    }    
}
