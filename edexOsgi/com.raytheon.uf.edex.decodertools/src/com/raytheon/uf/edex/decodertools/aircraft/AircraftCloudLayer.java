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
 * [Class Description]
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			PR#			Engineer		Description
 * -----------	----------	------------	--------------------------
 * Mar 9, 2005				dweeks			Initial development : JET
 * 20071227            384  jkorman         Ported to edex.
 * </pre>
 * @author dweeks
 * @version [version] Mar 9, 2005
 */
public class AircraftCloudLayer
{
    private String theCloudCover1 = null;
    private String theCloudCover2 = null;
    private Integer theCloudBaseHeight = null;
    private Integer theCloudTopHeight = null;

    /**
     * @return Returns the theCloudBaseHeight.
     */
    public Integer getCloudBaseHeight()
    {
        return theCloudBaseHeight;
    }
    /**
     * @param theCloudBaseHeight The theCloudBaseHeight to set.
     */
    public void setCloudBaseHeight(Integer theCloudBaseHeight)
    {
        this.theCloudBaseHeight = theCloudBaseHeight;
    }
    /**
     * @return Returns the theCloudCover1.
     */
    public String getCloudCover1()
    {
        return theCloudCover1;
    }
    /**
     * @param theCloudCover1 The theCloudCover1 to set.
     */
    public void setCloudCover1(String theCloudCover)
    {
        this.theCloudCover1 = theCloudCover;
    }
    /**
     * @return Returns the theCloudCover2.
     */
    public String getCloudCover2()
    {
        return theCloudCover2;
    }
    /**
     * @param theCloudCover2 The theCloudCover2 to set.
     */
    public void setCloudCover2(String theCloudCover2)
    {
        this.theCloudCover2 = theCloudCover2;
    }
    /**
     * @return Returns the theCloudTopHeight.
     */
    public Integer getCloudTopHeight()
    {
        return theCloudTopHeight;
    }
    /**
     * @param theCloudTopHeight The theCloudTopHeight to set.
     */
    public void setCloudTopHeight(Integer theCloudTopHeight)
    {
        this.theCloudTopHeight = theCloudTopHeight;
    }
}
