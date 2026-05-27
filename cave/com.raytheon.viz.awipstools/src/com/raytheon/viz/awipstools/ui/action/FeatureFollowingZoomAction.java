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

import com.raytheon.viz.ui.actions.LoadBundleHandler;

/**
 * Action for feature follow zoom
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 30, 2013  2310     bsteffen    Rewritten to extend LoadBundleHandler.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 * @deprecated Use {@link LoadBundleHandler} with
 *             bundleFile="bundles/tools/FeatureFollowingZoom.xml".
 */
@Deprecated
public class FeatureFollowingZoomAction extends LoadBundleHandler {

    public FeatureFollowingZoomAction() {
        super("bundles/tools/FeatureFollowingZoom.xml");
    }

}
