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
package com.raytheon.uf.viz.points.ui.action;

import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.uf.viz.points.ui.layer.PointsToolLayer;

/**
 * Handles the Points Tool Action.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Oct032007    #455        ebabin      Initial Creation.
 *  14Oct2009    #810        bsteffen    Fix for grabbing points.
 *  10-21-09     #1711       bsteffen    Refactor to common MovableTool model
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class PointsToolAction extends AbstractGenericToolAction<PointsToolLayer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<PointsToolLayer> getResourceData() {
        return new GenericToolsResourceData<PointsToolLayer>(
                PointsToolLayer.DEFAULT_NAME, PointsToolLayer.class);

    }
}
