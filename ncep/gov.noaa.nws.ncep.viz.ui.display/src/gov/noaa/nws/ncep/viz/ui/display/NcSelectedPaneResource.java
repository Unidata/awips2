package gov.noaa.nws.ncep.viz.ui.display;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;

/**
 * A system resource to draw the yellow highlight to indicate the selected panes in 
 * the NC Perspective 
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 * 03/07/11         R1G2-9      Greg Hull    Created                            
 * 
 * </pre>
 * 
 * @author ghull
 * 
 */
public class NcSelectedPaneResource extends 
				AbstractVizResource<GenericResourceData, IDescriptor> {

	private AbstractNcEditor ncMapEditor = null;
	
    protected static final int HIGHLIGHT_WIDTH = 2;

    protected static final RGB HIGHLIGHT_COLOR = new RGB(255, 255, 0);

    public NcSelectedPaneResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void disposeInternal() {
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
    	
    	// don't draw the highlight if there is only 1 pane
    	if( ncMapEditor == null ||
    		ncMapEditor.getDisplayPanes().length <= 1 ) {
    		return;
    	}
    	
    	// If the pane for this descriptor is a selected pane then 
    	// draw the highlight border around the edge of the pane.
    	//
    	IDisplayPane[] seldPanes = NcEditorUtil.getSelectedPanes(ncMapEditor);
    	
    	IDisplayPane thisSeldPane = null;
            
    	for (IDisplayPane pane : seldPanes ) {
    		if (pane.getDescriptor() == descriptor) {
    			thisSeldPane = pane;
    			break;
    		}
    	}
    	
    	if( thisSeldPane != null) {
    		IExtent extent = paintProps.getView().getExtent();

    		target.clearClippingPlane();

    		target.drawRect( extent, HIGHLIGHT_COLOR, 5, 1.0 );
    		
    		target.setupClippingPlane(extent);    	
    	}
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        IDisplayPaneContainer container = getResourceContainer();
        if( container instanceof AbstractNcEditor ) {
        	ncMapEditor = (AbstractNcEditor)container;
        }
    }

    @Override
    public ResourceOrder getResourceOrder() {
        return ResourceOrder.HIGHEST;
    }

}
