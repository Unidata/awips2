package gov.noaa.nws.ncep.viz.ui.seek;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcModalTool;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
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
 * May 	 2012	# 747	  B. Yin		Made the pan tool work when the shift key is held down.     
 * Feb   2012   #972      G. Hull      don't implement for NTRANS displays                                             
 *   
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class SeekResultsAction extends AbstractNcModalTool  {
	public static boolean addedSeekLayerToResourceList = false;
	protected IInputHandler mouseHandler;
	protected SeekResourceData seekResourceData;
	protected SeekDrawingLayer seekDrawingLayer;
	
	protected SeekResultsDialog seekRsltsDlg;
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    protected void activateTool() {
    	
    	/*
         * Register mouse handler. 
         */
    	mapEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	NcDisplayType dispType = NcEditorUtil.getNcDisplayType( mapEditor );
    	
    	// NOTE : Disable for NTRANS and SWPC 
    	if( !dispType.equals( NcDisplayType.NMAP_DISPLAY ) ) {
    		deactivateTool();
    		return;
    	}
    	
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
        if(seekRsltsDlg == null) { seekRsltsDlg = SeekResultsDialog.getInstance(shell, this);}
        if( ! seekRsltsDlg.isDlgOpen() ){ 
        	//initializeTheSeekLayer();
        	seekRsltsDlg.open(); 
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

    	private Coordinate firstMouseClick;
    	
    	private Coordinate[] endpts = new Coordinate[2];
    	
    	private boolean shiftDown;
    	private boolean simulate;

    	/*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
    	@Override
    	public boolean handleMouseDown(int x, int y, int button) {

    		if (button != 1 || simulate ) {
    			return false;
    		}
    		if (mapEditor != null ) {
    			Coordinate ll = mapEditor.translateClick(x, y);
    			firstMouseClick = ll;
    			if ( ll == null ) return false;
    		
    			if (seekRsltsDlg != null && seekRsltsDlg.isDlgOpen()/*.isOpen()*/ && ll != null) {//archana - changed isOpen() to isDlgOpen() 
    				seekRsltsDlg.setPosition(ll);
    				endpts = seekRsltsDlg.getEndPoints();
    				if (endpts[0] != null || endpts[1] != null) {
    					//seekDrawingLayer.drawClickPtLine(endpts[0], endpts[1]);
    					(( SeekResourceData) seekDrawingLayer.getResourceData()).setFirstPt(endpts[0]);
    					(( SeekResourceData) seekDrawingLayer.getResourceData()).setLastPt(endpts[1]);
    				}
    			}
    			mapEditor.refresh();
    		}
    		return false;
    	}

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
    	@Override
    	public boolean handleMouseDownMove(int x, int y, int button) {
    		if (button != 1 || shiftDown ) {
    			return false;
    		}
    		if (mapEditor != null){
    			Coordinate c1 = firstMouseClick;
    			Coordinate c2 = mapEditor.translateClick(x, y);
    			if (seekRsltsDlg != null && seekRsltsDlg.isDlgOpen() && c1 != null && c2 != null) {
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
                
    				double firstScnPt[] = mapEditor.translateInverseClick(firstMouseClick);
    				
    				Coordinate c = mapEditor.translateClick(firstScnPt[0] - 15, firstScnPt[1] - 15);
    				String str = seekRsltsDlg.getFormatDistance(distanceInMeter, azimuth);
                
//            	    seekDrawingLayer.clearStrings();
//            	    if (str != null) seekDrawingLayer.drawString(c, str);
    				((SeekResourceData)seekDrawingLayer.getResourceData()).clearStrings();
    				if (str != null) {
    					(( SeekResourceData) seekDrawingLayer.getResourceData()).drawString(c, str);
    				}  
    				mapEditor.refresh();
    			
    				//simulate a mouse down event so that the pan tool gets the location of the last click.
    				Event me = new Event();
    				me.display = mapEditor.getActiveDisplayPane().getDisplay();
    				me.button = 1;
    				me.type = SWT.MouseDown;
    				me.x = x;
    				me.y = y;
    				
    				simulate = true; 
    				mapEditor.getMouseManager().handleEvent(me);
    				simulate = false;
    				
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
        
        
    	@Override
    	public boolean handleKeyDown(int keyCode) {
    		if ( keyCode == SWT.SHIFT) {
    			shiftDown = true;
    		}
    	
    		return true;
    	}

    	@Override
    	public boolean handleKeyUp(int keyCode) {
    		if ( keyCode == SWT.SHIFT) {
    			shiftDown = false;
    		}
    		return true;
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
    			seekDrawingLayer = seekResourceData.construct(new LoadProperties(), 
    					NcEditorUtil.getDescriptor( mapEditor ) );
    			seekDrawingLayer.init(editor.getActiveDisplayPane()
    					.getTarget());

    			addedSeekLayerToResourceList =  
    				NcEditorUtil.getDescriptor( mapEditor ).getResourceList().add(
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
    		  NcEditorUtil.getDescriptor( mapEditor ).getResourceList().removeRsc(seekDrawingLayer);
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
