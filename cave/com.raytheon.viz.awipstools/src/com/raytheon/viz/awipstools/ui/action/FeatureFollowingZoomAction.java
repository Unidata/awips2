/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.ui.action;

import com.raytheon.uf.viz.core.rsc.tools.AwipsToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractMapToolAction;
import com.raytheon.viz.awipstools.ui.layer.FeatureFollowingZoomLayer;

/**
 * Action for feature follow zoom
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class FeatureFollowingZoomAction extends
        AbstractMapToolAction<FeatureFollowingZoomLayer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected AwipsToolsResourceData<FeatureFollowingZoomLayer> getResourceData() {
        return new AwipsToolsResourceData<FeatureFollowingZoomLayer>(
                "Feature Following Zoom", FeatureFollowingZoomLayer.class);
    }
}
