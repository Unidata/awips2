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
package com.raytheon.viz.ui.perspectives;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

/**
 * Interface for customizing renderable displays for a perspective when a new
 * renderable display is created in the perspective
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2013       2190 mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IRenderableDisplayCustomizer {

    /**
     * Method for customizing the a renderable display
     * 
     * @param display
     */
    public void customizeDisplay(IRenderableDisplay display);

    /**
     * Method for undoing customizations done to the renderable display from
     * {@link #customizeDisplay(IRenderableDisplay)}
     * 
     * @param display
     */
    public void uncustomizeDisplay(IRenderableDisplay display);

}
