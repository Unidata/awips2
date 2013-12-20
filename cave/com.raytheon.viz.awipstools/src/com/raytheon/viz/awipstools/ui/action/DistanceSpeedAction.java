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

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.awipstools.ui.layer.DistanceSpeedLayer;

/**
 * Handles Distance Speed tool creation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct172007    #495        ebabin      Initial Creation.
 * Feb152011    #7975       bkowal      Restore the DistanceSpeedLayer
 *                                      associated with the Display Pane.
 * 2013-12-20   ss#114      D. Friedman Change 1 of 3
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class DistanceSpeedAction extends
        AbstractGenericToolAction<DistanceSpeedLayer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<DistanceSpeedLayer> getResourceData() {
        return new GenericToolsResourceData<DistanceSpeedLayer>(
                DistanceSpeedLayer.NAME, DistanceSpeedLayer.class);
    }

    @Override
    protected DistanceSpeedLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        DistanceSpeedLayer layer = null;

        for (IDisplayPane pane : getSelectedPanes()) {
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getResource() instanceof DistanceSpeedLayer) {
                    layer = (DistanceSpeedLayer) rp.getResource();
                    layer.reopenDialog();
                    return layer;
                }
            }
        }

        layer = super.getResource(loadProperties, descriptor);
        return layer;
    }

}
