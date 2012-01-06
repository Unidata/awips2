package gov.noaa.nws.ncep.viz.tools.panZoom;

import java.util.HashMap;

import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.GraphicsFactory;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.input.InputAdapter;


public class NcZoomHandler extends InputAdapter {

    private static final NcZoomHandler instance = new NcZoomHandler();

    private int firstX, firstY;

    private Rectangle zoomRect;

    private IDisplayPane activePane;

    private ResourcePair resource;

    private NcZoomToolResourceData ztrd;

    private IDisplayPaneContainer container = null;

    private NcZoomHandler() {
        ztrd = new NcZoomToolResourceData(this);
        resource = ResourcePair.constructSystemResourcePair(ztrd);
    }

    public static NcZoomHandler getInstance(IDisplayPaneContainer container) {
        instance.container = container;
        return instance;
    }

    public Rectangle getZoomRect() {
        return zoomRect;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int aX, int aY, int button) {
    	//System.out.println("handleMouseDown for ZoomHandler");
    	
        if (button != 1) {
            return false;
        }
        else if( !(container instanceof NCMapEditor) ) {
        	return false;
        }
        
        // use the last selectd pane.
        IDisplayPane[] seldPanes = ((NCMapEditor)container).getSelectedPanes();        
        activePane = seldPanes[ seldPanes.length-1 ];
        
        if (activePane == null) {
            return false;
        }
        
        // Add zoom tool resource to active pane
        activePane.getDescriptor().getResourceList().add(resource);
        activePane.getDescriptor().getResourceList()
        					.instantiateResources( activePane.getDescriptor(), true );
        container.refresh();

        firstX = aX;
        firstY = aY;
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    public boolean handleMouseDownMove(int aX, int aY, int button) {
        if (button != 1 || activePane == null) {
            return false;
        }
        setZoombox(firstX, firstY, aX, aY);
        container.refresh();
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     */
    @Override
    public boolean handleMouseUp(int aX, int aY, int button) {
        if (button != 1 || activePane == null) {
            return false;
        }
        else if( !(container instanceof NCMapEditor) ) {
        	return false;
        }
        
        NCMapEditor ncEditor = (NCMapEditor)container; 
        IDisplayPane[] zoomPanes = ( ncEditor.arePanesGeoSynced() ? 
        	container.getDisplayPanes() : ncEditor.getSelectedPanes() );

        for (IDisplayPane pane : zoomPanes ) {
            zoomToZoombox(pane);
        }

        // Remove zoom tool resource from active pane
        activePane.getDescriptor().getResourceList().remove(resource);
        activePane = null;
        zoomRect = null;
        
        // after the zoom is done change back to Pane Mode.
        //
		String cmdStr = ncEditor.getDefaultTool();        		
		ICommandService service = (ICommandService) ncEditor.getSite()
									   .getService( ICommandService.class );
		Command cmd = service.getCommand( cmdStr );			

		if ( cmd != null ) {
			try {
				HashMap<String, Object> params = new HashMap<String, Object>();
//				params.put( "editor",  );
//				params.put("name", elem.getAttribute("name"));
//				params.put("className", elem.getAttribute("className"));
				ExecutionEvent exec = new ExecutionEvent( cmd, params, null, null );
//						elem.getAttribute("name") );

				cmd.executeWithChecks( exec );
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Error executing cmd: "+ cmdStr );
			}
		}                                

		ncEditor.refresh();

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#setZoombox(int, int, int, int)
     */
    public void setZoombox(int firstX, int firstY, int lastX, int lastY) {
        IExtent extent = activePane.getRenderableDisplay().getExtent();
        Rectangle bounds = activePane.getBounds();

        int correctedX = (int) ((firstX * (extent.getMaxX() - extent.getMinX()) / bounds.width) + extent
                .getMinX());
        int correctedX2 = (int) ((lastX * (extent.getMaxX() - extent.getMinX()) / bounds.width) + extent
                .getMinX());
        int correctedY = (int) ((firstY * (extent.getMaxY() - extent.getMinY()) / bounds.height) + extent
                .getMinY());
        int correctedY2 = (int) ((lastY * (extent.getMaxY() - extent.getMinY()) / bounds.height) + extent
                .getMinY());

        this.zoomRect = new Rectangle(correctedX, correctedY, correctedX2
                - correctedX, correctedY2 - correctedY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#zoomToZoombox()
     */
    public void zoomToZoombox(IDisplayPane pane) {

        if (zoomRect == null) {
            return;
        }

        Rectangle curDisplay = pane.getBounds();

        double ratioX = (double) this.zoomRect.width
                / (double) curDisplay.width;
        double ratioY = (double) this.zoomRect.height
                / (double) curDisplay.height;

        double newRatio = 0.0;
        if (ratioX > ratioY) {
            newRatio = ratioX;
        } else {
            newRatio = ratioY;
        }

        double wd = (curDisplay.width * newRatio);
        double ht = (int) (curDisplay.height * newRatio);

        int centerX = this.zoomRect.x + this.zoomRect.width / 2;
        int centerY = this.zoomRect.y + this.zoomRect.height / 2;
        IExtent extent = null;

        try {
            extent = GraphicsFactory.getGraphicsAdapter().constructExtent(
                    centerX - wd / 2, centerX + wd / 2, centerY - ht / 2,
                    centerY + ht / 2);
        } catch (VizException e) {
            /*
             * Failed to construct extent with the factory. Default to
             * PixelExtent type.
             */
            extent = new PixelExtent(centerX - wd / 2, centerX + wd / 2,
                    centerY - ht / 2, centerY + ht / 2);
        }

        pane.getRenderableDisplay().setExtent(extent);
        pane.setZoomLevel(pane.getRenderableDisplay().recalcZoomLevel(
                pane.getRenderableDisplay().getDimensions()));

        pane.refresh(); 
    }

}
