package gov.noaa.nws.ncep.viz.ui.seek;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Popup SEEK results dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * March 2009  	86        M. Li    		Initial creation. 
 * Sept  2009   169       G. Hull       AbstractNCModalMapTool
 * Dec   2010   351      Archana      Removed getSeekLayer()
 *                                                        Added logic to initializeTheSeekLayer()
 *                                                        such that the seekDrawingLayer is created afresh
 *                                                        for each descriptor
 *                                                        Moved the data associated with the seek resource (seekDrawingLayer)
 *                                                        to the seekResourceData object
 *                                                        Updated the execute() method to toggle
 *                                                        the display of the seek layer.  
 * Jan   2012   TTR 326   J. Zeng      handled NUllPointerException                                                      
 *   
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class SeekResultsAction extends AbstractNCModalMapTool  {
	public static boolean addedSeekLayerToResourceList = false;
	protected IInputHandler mouseHandler;
	protected SeekResourceData seekResourceData;
	protected SeekDrawingLayer seekDrawingLayer;
	
	protected SeekResultsDialog id;
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    protected void activateTool() {
    	
    	/*
         * Register mouse handler. 
         */
    	mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
        if ( mouseHandler == null ) {
            mouseHandler = createSeekMouseHandler();
        }
        if (mapEditor != null)  {
        	mapEditor.registerMouseHandler( this.mouseHandler );
        }
        
        initializeTheSeekLayer();
        
        /*
         * Pop up Seek result window
         */
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        if(id == null) { id = SeekResultsDialog.getInstance(shell, this);}
        if( ! id.isDlgOpen() ){ 
        	//initializeTheSeekLayer();
        	id.open(); 
        }
    }
    
    /*
     * (non-Javadoc)
     * org.osgi.framework.BundleContext
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
    	
        if (mapEditor != null && mouseHandler != null){
            mapEditor.unregisterMouseHandler( this.mouseHandler );
            mouseHandler = null;
        }
        removeSeekLayer();
    }

    
    public class SeekMouseHandler extends InputAdapter {

    	private int firstMouseX;

    	private int firstMouseY;
    	
    	private Coordinate[] endpts = new Coordinate[2];

    	/*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
    	@Override
    	public boolean handleMouseDown(int x, int y, int button) {

    		if (button != 1) {
    			return false;
    		}
    		if (mapEditor != null ) {
    			Coordinate ll = mapEditor.translateClick(x, y);
    			if ( ll == null ) return false;
    		
    			if (id != null && id.isDlgOpen()/*.isOpen()*/ && ll != null) {//archana - changed isOpen() to isDlgOpen() 
    				id.setPosition(ll);
    				firstMouseX = x;
    				firstMouseY = y;
    				endpts = id.getEndPoints();
    				if (endpts[0] != null || endpts[1] != null) {
    					//seekDrawingLayer.drawClickPtLine(endpts[0], endpts[1]);
    					(( SeekResourceData) seekDrawingLayer.getResourceData()).setFirstPt(endpts[0]);
    					(( SeekResourceData) seekDrawingLayer.getResourceData()).setLastPt(endpts[1]);
    				}
    			}
    			mapEditor.refresh();
    		}
    		return true;
    	}

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
    	@Override
    	public boolean handleMouseDownMove(int x, int y, int button) {
    		if (button != 1) {
    			return false;
    		}
    		if (mapEditor != null){
    			Coordinate c1 = mapEditor.translateClick(firstMouseX, firstMouseY);
    			Coordinate c2 = mapEditor.translateClick(x, y);
    			if (id != null && id.isDlgOpen() && c1 != null && c2 != null) {
 //   				seekDrawingLayer.drawLine(c1, c2);
    				(( SeekResourceData) seekDrawingLayer.getResourceData()).setPoint1(c1);
    				(( SeekResourceData) seekDrawingLayer.getResourceData()).setPoint2(c2);
    				// Calculate distance and direction
    				GeodeticCalculator gc = new GeodeticCalculator(
                        DefaultEllipsoid.WGS84);
    				gc.setStartingGeographicPoint(c2.x, c2.y);
    				gc.setDestinationGeographicPoint(c1.x, c1.y);
                
    				double azimuth = gc.getAzimuth();
    				if (azimuth < 0) azimuth += 360.0;
    				double distanceInMeter = gc.getOrthodromicDistance();
                
    				Coordinate c = mapEditor.translateClick(firstMouseX - 15, firstMouseY - 15);
    				String str = id.getFormatDistance(distanceInMeter, azimuth);
                
//            	    seekDrawingLayer.clearStrings();
//            	    if (str != null) seekDrawingLayer.drawString(c, str);
    				((SeekResourceData)seekDrawingLayer.getResourceData()).clearStrings();
    				if (str != null) {
    					(( SeekResourceData) seekDrawingLayer.getResourceData()).drawString(c, str);
    				}  
    				mapEditor.refresh();
    				return true;
    			}
    		}
    		return false;
    	}

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         *      int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button) {
        	if (button != 1) {
                return false;
            }
        	if (mapEditor != null){
        		Coordinate ll = mapEditor.translateClick(x, y);
        		if ( ll == null ) return false;        	
//        		seekDrawingLayer.clearStrings();
        		(( SeekResourceData) seekDrawingLayer.getResourceData()).clearStrings();
//        		seekDrawingLayer.clearLine();
        		(( SeekResourceData) seekDrawingLayer.getResourceData()).clearLine();
        		mapEditor.refresh();
        		return true;
        	}
        	return false;
        	
        }
    }   
    
//    private SeekDrawingLayer getSeekLayer() {
//    	// See if an seek drawing layer is there
//        ResourceList rscs = mapEditor.getDescriptor().getResourceList();
//        for (ResourcePair r : rscs) {
//            if (r.getResource() instanceof SeekDrawingLayer) {
//                seekDrawingLayer = (SeekDrawingLayer) r.getResource();
//                break;
//            }
//        }
//        if(seekDrawingLayer == null){
//        	initializeTheSeekLayer();
//        }
//   
//        return seekDrawingLayer;
//    }
    
    protected void initializeTheSeekLayer(){
    	if (mapEditor !=  null){
    		try {
    			if ( seekResourceData == null ){
    				seekResourceData = new SeekResourceData();
    		}

    		if ( seekDrawingLayer == null ){
    			seekDrawingLayer = seekResourceData.construct(new LoadProperties(), mapEditor.getDescriptor());
    			seekDrawingLayer.init(mapEditor.getActiveDisplayPane()
    					.getTarget());

    			addedSeekLayerToResourceList =  mapEditor.getDescriptor().getResourceList().add(
    					seekDrawingLayer);
    		}
    		} catch (VizException e) {
    			e.printStackTrace();
    		} catch (NullPointerException e){
    			e.printStackTrace();
    		}
    		mapEditor.refresh();
    	}
    }
    
    protected /*private*/ void removeSeekLayer() {
    	
    	
      if(seekDrawingLayer != null){
    	  /*save off the resource data for the next time the handler is activated*/
 //   	  seekResourceData = ( SeekResourceData) seekDrawingLayer.getResourceData(); 
    	  if (mapEditor != null ){
    		  mapEditor.getDescriptor().getResourceList().removeRsc(seekDrawingLayer);
    		  addedSeekLayerToResourceList = false;
//    	 seekDrawingLayer.disposeInternal(); /*Added by archana*/
    		  seekDrawingLayer = null;
    		  mapEditor.refresh();
    	  }
      }
//    	
    }
    
    protected SeekMouseHandler createSeekMouseHandler(){
    	return  (new SeekMouseHandler());
    }

	public String getCommandId() {
		return commandId;
	}
    
}
