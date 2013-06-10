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
import com.raytheon.viz.awipstools.ui.layer.AzimuthToolLayer;

/**
 * Show 'Az/Ran' Overlay.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Sep142007    #444        ebabin      Initial Creation.
 *  May282010    #5361       bkowal      We now pass the mouseButton
 *                                       identifier to the Azimuth Tool Layer
 *                                       handle mouse button function.
 *  Jun142010    #6360       bkowal      Ensured that the legend will no longer
 *                                       change when the user changes the position
 *                                       of the tool with the right-mouse button.
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class AzimuthToolAction extends AbstractGenericToolAction<AzimuthToolLayer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<AzimuthToolLayer> getResourceData() {
        return new GenericToolsResourceData<AzimuthToolLayer>(
                AzimuthToolLayer.AZIMUTH_LOCATION, AzimuthToolLayer.class);
    }

}
