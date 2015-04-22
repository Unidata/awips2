package gov.noaa.nws.ncep.viz.tools.panZoom;

import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.viz.core.GraphicsFactory;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * handler to zoom to a given zoom level
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *   11/18/12   #630       G. Hull      
 *   02/22/13   #972       G. Hull       AbstractNcEditor instead of NCMapEditor
 *   05/15/13   #862       G. Hull      Resources as AreaProviders
 *   11/24/13   #1078      G. Hull      diff constructor for PixelExtent on sizeOfImage
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ZoomToAction extends AbstractHandler {

	public static enum ZoomType {
		ZOOMBOX,   //  
		AREA_PROVIDER,  // from a satellite or other resource that can specify an area
		ZOOM_LEVEL
	}

    public ZoomToAction() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        String zoomLevelStr = null;        
        String zoomType = null;

        try {
        	try {
        		zoomType = event.getParameter("zoomType");
        		zoomLevelStr = event.getParameter("zoomLevel");
        	} catch (Exception e) {
        		throw new VizException("zoomTo command parameter not set???");
        	}

        	// if zooming to an area defined by an areaProvider (ie. sat rsc), 
        	// then the zoom level is the name of the area.
        	if( zoomType.equals( ZoomType.AREA_PROVIDER.toString() ) ) {        		
        		zoomToArea( AreaName.parseAreaNaem( zoomLevelStr ) );
        	}
        	else if( zoomType.equals( ZoomType.ZOOM_LEVEL.toString() ) ) {
        		if( zoomLevelStr.equals("+") || zoomLevelStr.equals("-") ) {
        			
        		}
        		else {
//        			int zoomLevel = Integer.parseInt( zoomLevelStr );
        		}
        		System.out.println("Zoom Level not implemented");
        	}
        	else if( zoomType.equals( ZoomType.ZOOMBOX.toString() ) ) {
        		//"Rectangle {" + x + ", " + y + ", " + width + ", " + height + "}";
        		String rectStr = zoomLevelStr.substring( "Rectangle {".length(),
        				         zoomLevelStr.length()-1 );
        		String[] rectCoordsStrs = rectStr.split(",");
        		
        		if( rectCoordsStrs.length != 4 ) {
        			throw new VizException("Error parsing zoomBox from, "+zoomLevelStr );
        		}
        		try {
        			Rectangle zoomRect = new Rectangle( 
        					Integer.parseInt( rectCoordsStrs[0].trim() ),
        					Integer.parseInt( rectCoordsStrs[1].trim() ),
        					Integer.parseInt( rectCoordsStrs[2].trim() ),
        					Integer.parseInt( rectCoordsStrs[3].trim() ) );
        			
        			zoomToZoombox( zoomRect );
        		}
        		catch( NumberFormatException nfe ) {
        			throw new VizException( nfe );
        		}
        	}
        	else {
        		throw new VizException("Unknown zoomType command param:	"+zoomType );
        	}

        } catch (VizException e) {        	
        	MessageDialog errDlg = new MessageDialog( 
        			NcDisplayMngr.getCaveShell(), "Error", null, 
        			"Error Zooming:\n\n"+e.getMessage(),
        			MessageDialog.ERROR, new String[]{"OK"}, 0);
        	errDlg.open();
        }

        return null;
    }

    /**
     * @param areaName
     * @throws VizException 
     */
    public static void zoomToArea( AreaName areaName ) throws VizException {
    	
    	IGridGeometryProvider geomProv = NcAreaProviderMngr.createGeomProvider( areaName );

    	// get the pane of the selected resource.
    	AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();

    	// get the panes to set the area in.
    	IDisplayPane pane = (IDisplayPane)editor.getActiveDisplayPane();

    	// Note : currently this only makes sense for a MapRenderableDisplay
    	
    	INatlCntrsRenderableDisplay rendDisp = (INatlCntrsRenderableDisplay)pane.getRenderableDisplay();

    	if( !(rendDisp instanceof NCMapRenderableDisplay ) ) {
    		throw new VizException( "Error: can't zoom to resource in the renderable display : "+
    				rendDisp.getClass().getName() );
    	}
    	
    	NCMapDescriptor mapDescr = ((NCMapRenderableDisplay)rendDisp).getDescriptor();    	
    	
//    	pane.scaleToClientArea();
//    	existingDisplay.recenter( existingDisplay.getMapCenter() );
//    	existingDisplay.getView().zoom( pArea.getZoomLevel() );
//
//    	((NCMapDescriptor)existingDisplay.getDescriptor()).setSuspendZoom(false);
        GeneralGridGeometry gridgeom = geomProv.getGridGeometry(); 
        int xdimArea = gridgeom.getGridRange().getSpan(0);
        int ydimArea = gridgeom.getGridRange().getSpan(1);

        try {
            mapDescr.setGridGeometry( new GridGeometry2D(
                    new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                            xdimArea, ydimArea }, false), gridgeom
                            .getEnvelope()));

            double[] center = mapDescr.pixelToWorld(new double[] {
                    xdimArea / 2, ydimArea / 2, 0. });

            NCMapRenderableDisplay newDisplay = new NCMapRenderableDisplay();
//            							rendDisp.getPaneId() );//, mapDescr );
            newDisplay.setPaneId( rendDisp.getPaneId() );
            newDisplay.setDescriptor( mapDescr );

            // When PixelExtent is constructed from a Rectangle, its aMinX... members 
            // are not set. When its clone() is called later the envelope is created
            // using the aMinX...   members which causes mucho problemos.
			Rectangle rect = pane.getBounds();
            newDisplay.setExtent( 
            		new PixelExtent( //pane.getBounds() ) );
            				rect.x, rect.x + rect.width,
            				rect.y, rect.y + rect.height ) );
            
            mapDescr.setSuspendZoom( true );

            newDisplay.setMapCenter(center);
            pane.setRenderableDisplay( newDisplay );
            
//          ZoomUtil.suspendZoom( editor) ;
            NcEditorUtil.refreshGUIElements( editor );

        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    	VizGlobalsManager.getCurrentInstance().updateUI(editor);

    	editor.refresh();
    }
    
//    private PredefinedArea getZoomLevelFromResource( 
//    		NCMapEditor editor, ResourceName rscName ) throws VizException {
//
//    	NCDisplayPane[] displayPanes = (NCDisplayPane[])editor.getDisplayPanes();
//    	 
//    	for( NCDisplayPane p : displayPanes ) {
//
//    		ResourceList rlist = p.getDescriptor().getResourceList();
//    		Iterator<ResourcePair> iter = rlist.iterator();
//
//    		while( iter.hasNext()) {
//    			ResourcePair rp = iter.next();
//
//    			if( rp.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData &&
//    					rp.getResourceData() instanceof IGridGeometryProvider ) {
//    				ResourceName rName = ((AbstractNatlCntrsRequestableResourceData)rp.getResourceData()).getResourceName();
//
//    				if( rscName.equals( rName ) ) {
//    					IGridGeometryProvider gridCovRsc = (IGridGeometryProvider)rp.getResourceData();
////    					if( gridCovRsc.getGridGeometry() != null ) {
//    					PredefinedArea pArea = new PredefinedArea( 
//    			        		AreaType.RESOURCE_DEFINED,
//    			        		gridCovRsc.getProviderName(), 
//    			        		gridCovRsc.getGridGeometry(),
//    			        		gridCovRsc.getMapCenter(),
//    			        		gridCovRsc.getZoomLevel() );
//
//    						return pArea; //.getGridGeometry();
////    					}
//    				}
//    			}
//    		}
//    	}
//    	throw new VizException( "Unable to change the Display Area. \n"
//    				+ "The Resource "+rscName.toString()+" is not loaded in this editor.\n");
//    }
    
    public void zoomToZoombox( Rectangle zoomRect) {
        
    	AbstractEditor ed = NcDisplayMngr.getActiveNatlCntrsEditor();

    	IDisplayPane[] zoomPanes = ( NcEditorUtil.arePanesGeoSynced( ed   ) ? 
    		  ed.getDisplayPanes() : NcEditorUtil.getSelectedPanes(ed) );

    	for( IDisplayPane pane : zoomPanes ) {

    		Rectangle bounds = pane.getBounds();

    		double ratioX = (double) zoomRect.width  / (double) bounds.width;
    		double ratioY = (double) zoomRect.height / (double) bounds.height;

    		double newRatio = 0.0;
    		if (ratioX > ratioY) {
    			newRatio = ratioX;
    		} else {
    			newRatio = ratioY;
    		}

    		double wd = (bounds.width * newRatio);
    		double ht = (int) (bounds.height * newRatio);

    		int centerX = zoomRect.x + zoomRect.width / 2;
    		int centerY = zoomRect.y + zoomRect.height / 2;
    		IExtent extent = null;

    		try {
    			extent = GraphicsFactory.getGraphicsAdapter().constructExtent(
    					centerX - wd / 2, centerX + wd / 2, centerY - ht / 2,
    					centerY + ht / 2);
    		} catch (VizException e) {
    			//   	Failed to construct extent with the factory. Default to PixelExtent type.
    			extent = new PixelExtent(centerX - wd / 2, centerX + wd / 2,
    					centerY - ht / 2, centerY + ht / 2);
    		}

    		pane.getRenderableDisplay().setExtent(extent);
    		pane.setZoomLevel(pane.getRenderableDisplay().recalcZoomLevel(
    				pane.getRenderableDisplay().getDimensions()));

    		pane.refresh();
    	}
    }
}