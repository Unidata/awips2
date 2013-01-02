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
package com.raytheon.viz.mpe.ui.actions;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;

/**
 * Handler for saving best estimate bottom
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SaveBestEstimateBottom extends SaveBestEstimateHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.actions.SaveBestEstimate#getPaneToSave(com.raytheon
     * .uf.viz.core.IDisplayPaneContainer)
     */
    @Override
    protected IDisplayPane getPaneToSave(IDisplayPaneContainer container) {
        IDisplayPane[] panes = container.getDisplayPanes();
        if (panes.length == 2) {
            return panes[1];
        }
        return null;
    }

}
