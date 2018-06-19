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
 * Create a remarks instance. The remarks are any number of string fragments
 * separated by a space character.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * Feb 28, 2005        753 jkorman       Initial creation.
 * 20071227            384 jkorman       Ported to edex.
 * Sep 18, 2014       3627 mapeters      Removed unused insertRemarks().
 * 
 * </pre>
 */
public class AircraftRemarks
{
    // The remarks string being built.
    private StringBuffer theRemarks = new StringBuffer();
    
    /**
     * Create a remarks instance with a given remarks or remarks fragment. 
     * @param aRemark A remarks fragment to add.
     */
    public AircraftRemarks(String aRemark)
    {
        theRemarks.append(aRemark);
    } // AircraftRemarks()

    /**
     * Adds a remarks fragment at the end of the Remarks being built.
     * @param aRemark A remarks fragment to add.
     */
    public void addRemarks(String aRemark)
    {
        if(aRemark != null)
        {
            theRemarks.append(aRemark);
        }
    } // addRemarks()
    
    /**
     * Get the current remarks string being built.
     * @return The remarks string.
     */
    public String getValue()
    {
        return theRemarks.toString();
    } // getValue()
    
    /**
     * Get the current remarks string being built.
     * @return The remarks string.
     */
    public String toString()
    {
        return getValue();
    } // toString()
    
} // AircraftRemarks
