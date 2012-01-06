/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
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

package gov.noaa.nws.ncep.viz.ui.display;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.panes.VizDisplayPane;

/**
 * Creates a GL Context for drawing
 * 
 * <pre>
 * 
 *      SOFTWARE HISTORY
 *          
 *      Date         Ticket#     Engineer    Description
 *      ------------ ----------  ----------- --------------------------
 *      03/21/11     R1G2-9      Greg Hull   Initial Creation, derive from GLDisplayPane
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
// bsteffen now extends VizDisplayPane
// public class NCDisplayPane extends GLDisplayPane {
public class NCDisplayPane extends VizDisplayPane {

    public NCDisplayPane(IDisplayPaneContainer container, Composite c,
            IRenderableDisplay display, boolean enableContextualMenus)
            throws VizException {
        super(container, c, display, enableContextualMenus);
    }

    public NCDisplayPane(IDisplayPaneContainer container, Composite c,
            IRenderableDisplay display) throws VizException {
        super(container, c, display);
    }

    //
    // this will cause the resources to be removed from the ResourceCatalog
    // This would be a convenience for ResourceBndlLoader but since this is
    // the only thing in NCDisplayPane, leave it out.
    // public void clearResources() {
    // renderableDisplay.getDescriptor().getResourceList().clear();
    // }

}
