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
package com.raytheon.viz.awipstools.ui.action;

import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.awipstools.ui.layer.DistanceBearingToolLayer;

/**
 * Handles the Distance Bearing Tools Action.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Sep272007    #455        ebabin      Initial Creation.
 *  Oct012007    #471        ebabin      Clean up Clear handling.
 *  10-21-09     #1711       bsteffen    Refactor to common MovableTool model
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class DistanceBearingToolAction extends
        AbstractGenericToolAction<DistanceBearingToolLayer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<DistanceBearingToolLayer> getResourceData() {
        return new GenericToolsResourceData<DistanceBearingToolLayer>(
                DistanceBearingToolLayer.DEFAULT_NAME,
                DistanceBearingToolLayer.class);
    }

}
