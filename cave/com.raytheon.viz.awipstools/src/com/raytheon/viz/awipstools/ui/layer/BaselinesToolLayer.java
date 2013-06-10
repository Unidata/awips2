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
package com.raytheon.viz.awipstools.ui.layer;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;

/**
 * Strictly here so bundles don't break
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORYElement
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Sep192007    #447        ebabin      Initial Creation.
 *  26Oct2007    #504        ebabin      Update to use BaselineLoader class.
 *  20Dec2007    #645        ebabin      Updated to fix sampling.
 *  12May2008    #1031       ebabin      Fix for baselines editing.
 *  02sept2008   #1516       dhladky     de-JIBX baselines.
 *  14Oct2009    #683        bsteffen    Fix for grabbing points.
 *  10-21-09     #1711       bsteffen    Refactor to common MovableTool model
 *  06-09-10     #5360       bkowal      Added the ability to move an entire
 *                                       baseline when the right mouse button
 *                                       was clicked. And if the user clicked
 *                                       near a baseline that had just been moved,
 *                                       a new vertex will now be added to that
 *                                       baseline.
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class BaselinesToolLayer extends InteractiveBaselinesLayer {

    /**
     * @param resourceData
     * @param loadProperties
     */
    public BaselinesToolLayer(
            GenericToolsResourceData<InteractiveBaselinesLayer> resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

}
