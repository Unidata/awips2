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

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
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
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class TimeOfArrivalAction extends
        AbstractGenericToolAction<TimeOfArrivalLayer> {

    private TimeOfArrivalLayer layer = null;

    private GenericToolsResourceData<TimeOfArrivalLayer> data = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<TimeOfArrivalLayer> getResourceData() {
        if (data == null) {
            data = new GenericToolsResourceData<TimeOfArrivalLayer>(
                    TimeOfArrivalLayer.NAME, TimeOfArrivalLayer.class);
        }
        return data;
    }

    @Override
    protected TimeOfArrivalLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (layer == null || layer.getStatus() == ResourceStatus.DISPOSED) {
            layer = super.getResource(loadProperties, descriptor);
        } else {
            layer.reopenDialog();
        }
        return layer;
    }

}
