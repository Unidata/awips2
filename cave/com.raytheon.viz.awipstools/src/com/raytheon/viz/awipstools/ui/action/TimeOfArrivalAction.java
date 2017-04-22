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

import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.awipstools.ui.layer.TimeOfArrivalLayer;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	07DEC2007   #496       Eric Babin   Initial Creation.
 *  Apr 12 2013 DR 16032   D. Friedman Make it work in multiple panes.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class TimeOfArrivalAction extends
        AbstractGenericToolAction<TimeOfArrivalLayer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<TimeOfArrivalLayer> getResourceData() {
        return new GenericToolsResourceData<TimeOfArrivalLayer>(
                    TimeOfArrivalLayer.NAME, TimeOfArrivalLayer.class);
    }

    @Override
    protected TimeOfArrivalLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        TimeOfArrivalLayer layer = getExistingResource();
        if (layer == null)
            return super.getResource(loadProperties, descriptor);

        VizApp.runAsync( new Runnable() {
            @Override
            public void run() {
                TimeOfArrivalLayer layer = getExistingResource();
                if (layer != null) {
                    layer.makeEditableAndReopenDialog();
                }
            }
        });
        return layer;
    }

    private TimeOfArrivalLayer getExistingResource() {
        IDisplayPane[] panes = getSelectedPanes();
        if (panes != null && panes.length > 0) {
            List<TimeOfArrivalLayer> layers = null;
            layers = panes[0].getDescriptor().getResourceList()
                    .getResourcesByTypeAsType(TimeOfArrivalLayer.class);
            if (layers.size() > 0) {
                return layers.get(0);
            }
        }
        return null;
    }

}
