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
package com.raytheon.uf.viz.core.maps.scales;

/**
 * Interface for display containing a scale
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2013            mschenke    Initial creation
 * Oct 10, 2013       2104 mschenke    Added ability to change name of scale
 *                                     without affecting the projection
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IMapScaleDisplay {

    /**
     * Returns the name of the scale for the display
     */
    public String getScaleName();

    /**
     * Sets the name of the current scale. This should be nothing other than a
     * rename operation
     * 
     * @param scaleName
     */
    public void setScaleName(String scaleName);

    /**
     * Changes the scale of the display to the one passed in. This should cause
     * the display to internally change
     * 
     * @param scale
     */
    public void changeScale(String scaleName);
}
