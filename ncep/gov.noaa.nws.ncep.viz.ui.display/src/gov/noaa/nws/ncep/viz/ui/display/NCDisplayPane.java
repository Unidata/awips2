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

    @Override
    public void setRenderableDisplay( IRenderableDisplay renderableRsc ) {
    	super.setRenderableDisplay(renderableRsc);
    	
    }

    // if the user has locked the zoom due to a size-of-image reprojection, we
    // don't want to change the zoomLevel which is a done in VizPaneDisplay
    //   TODO : this seems to work except that resize() is called just after an rbd is
    // loaded which, when size of image is selected, causes the image not to display.
    // 
    public void resize() {
//    	if( !((NCMapDescriptor)getDescriptor()).getSuspendZoom() ) {
    		super.resize();
//    	}
    }
    
    // if we need to get rid of NCDisplayPane we can either go back 
    // to having a separate 'no-op' SuspendZoomHandler or add a check
    // for the descriptor's suspend zoom flag in our NcPanHandler class
    //
    @Override
    public void zoom(final int value, int mouseX, int mouseY) {
    	if( !((NCMapDescriptor)getDescriptor()).getSuspendZoom() ) {
    		super.zoom(value, mouseX, mouseY);
    	}
    }
    
    // resolve the zoomLevel if defined by a resource and set the predefined area in the Renderable display
    // if SizeOfImage is set then we need the current Bounds (canvas) in order to compute the zoomLevel
//    public void setPredefinedArea( PredefinedArea pArea ) throws VizException {
//
//    	//if( )
//    }
    
    //
    // this will cause the resources to be removed from the ResourceCatalog
    // This would be a convenience for ResourceBndlLoader but since this is
    // the only thing in NCDisplayPane, leave it out.
    // public void clearResources() {
    // renderableDisplay.getDescriptor().getResourceList().clear();
    // }

}
