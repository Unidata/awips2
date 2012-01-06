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
package com.raytheon.uf.edex.decodertools.aircraft;


/**
 * This class holds information for aircraft flight conditions, such as
 * turbulence and icing, that have a range of intensities, a type, and
 * layer information.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			PR#			Engineer		Description
 * -----------	----------	------------	--------------------------
 * Mar 14,2005				dweeks			Initial creation
 * 20071227            384 jkorman       Ported to edex.
 * 
 * </pre>
 * @author dweeks
 * @version [version] Mar 14, 2005
 */
public class AircraftFlightCondition
{
    /**
     * Range of intensities
     */
    protected String theIntensity1 = null;
    protected String theIntensity2 = null;
    
    private String frequency;
    
    /**
     * Type associated with the flight condition
     */
    protected String theType = null;
    
    /**
     * Altitude
     */
    private Integer theBaseHeight = null;
    private Integer theTopHeight = null;
    /**
     * @return Returns the theBaseHeight.
     */
    public Integer getBaseHeight()
    {
        return theBaseHeight;
    }
    /**
     * @param theBaseHeight The theBaseHeight to set.
     */
    public void setBaseHeight(Integer theBaseHeight)
    {
        this.theBaseHeight = theBaseHeight;
    }
    
    /**
     * @return the frequency
     */
    public String getFrequency() {
        return frequency;
    }
    /**
     * @param frequency the frequency to set
     */
    public void setFrequency(String frequency) {
        this.frequency = frequency;
    }
    /**
     * @return Returns the theIntensity1.
     */
    public String getIntensity1()
    {
        return theIntensity1;
    }
    /**
     * @param theIntensity1 The theIntensity1 to set.
     */
    public void setIntensity1(String theIntensity1)
    {
        this.theIntensity1 = theIntensity1;
    }
    /**
     * @return Returns the theIntensity2.
     */
    public String getIntensity2()
    {
        return theIntensity2;
    }
    /**
     * @param theIntensity2 The theIntensity2 to set.
     */
    public void setIntensity2(String theIntensity2)
    {
        this.theIntensity2 = theIntensity2;
    }
    /**
     * @return Returns the theTopHeight.
     */
    public Integer getTopHeight()
    {
        return theTopHeight;
    }
    /**
     * @param theTopHeight The theTopHeight to set.
     */
    public void setTopHeight(Integer theTopHeight)
    {
        this.theTopHeight = theTopHeight;
    }
    /**
     * @return Returns the theType.
     */
    public String getType()
    {
        return theType;
    }
    /**
     * @param theType The theType to set.
     */
    public void setType(String theType)
    {
        this.theType = theType;
    }
}
