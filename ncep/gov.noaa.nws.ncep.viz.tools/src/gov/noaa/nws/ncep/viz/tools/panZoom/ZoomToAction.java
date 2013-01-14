package gov.noaa.nws.ncep.viz.tools.panZoom;

import java.util.Iterator;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.PlatformUI;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.GraphicsFactory;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.core.CorePlugin;

/**
 * handler to zoom to a given zoom level
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 18, 2012   #630       G. Hull      
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ZoomToAction extends AbstractHandler {

	public static enum ZoomType {
		ZOOMBOX,   //  
		RESOURCE_DEFINED,  // from a satellite or other resource that can specify an area
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

        	if( zoomType.equals( ZoomType.RESOURCE_DEFINED.toString() ) ) {
        		ResourceName zoomRscName = new ResourceName( zoomLevelStr );
        		zoomToResource( zoomRscName );
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
        			NmapUiUtils.getCaveShell(), "Error", null, 
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
    public static void zoomToResource( ResourceName zoomRscName ) throws VizException {
    	// get the resource
    	// get the pane of the selected resource.
    	NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();

    	// get the panes to set the area in.
    	NCDisplayPane pane = (NCDisplayPane)editor.getActiveDisplayPane();

    	NCMapRenderableDisplay rendDisp = 
    		(NCMapRenderableDisplay) pane.getRenderableDisplay();

    	NCMapDescriptor mapDescr = rendDisp.getDescriptor();    	
    	ResourceList rList = mapDescr.getResourceList();
    	IGridGeometryProvider zoomRsc = null;
    
    	for( ResourcePair rp : rList ) {
    		if( rp.getResourceData() instanceof IGridGeometryProvider &&
    			zoomRscName.equals( 
    					((INatlCntrsResourceData)rp.getResourceData()).getResourceName() ) ) {
    				zoomRsc = (IGridGeometryProvider)rp.getResourceData();
    				break;
    		}
    	}
    
    	if( zoomRsc == null ) {
    		throw new VizException("Can't find resource, "+zoomRscName.toString() +
    				", to zoom to???");
    	}
    	
//    	pane.scaleToClientArea();
//    	existingDisplay.recenter( existingDisplay.getMapCenter() );
//    	existingDisplay.getView().zoom( pArea.getZoomLevel() );
//
//    	((NCMapDescriptor)existingDisplay.getDescriptor()).setSuspendZoom(false);
        GeneralGridGeometry gridgeom = zoomRsc.getGridGeometry(); 
        int xdimArea = gridgeom.getGridRange().getSpan(0);
        int ydimArea = gridgeom.getGridRange().getSpan(1);

        try {
            mapDescr.setGridGeometry( new GridGeometry2D(
                    new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                            xdimArea, ydimArea }, false), gridgeom
                            .getEnvelope()));

            double[] center = mapDescr.pixelToWorld(new double[] {
                    xdimArea / 2, ydimArea / 2, 0. });

            NCMapRenderableDisplay newDisplay = new NCMapRenderableDisplay(
            							rendDisp.getPaneId(), mapDescr );

            newDisplay.setExtent( new PixelExtent(pane.getBounds()) );
            mapDescr.setSuspendZoom( true );

            newDisplay.setMapCenter(center);
            pane.setRenderableDisplay( newDisplay );
            
//          ZoomUtil.suspendZoom( editor) ;
            editor.refreshGUIElements();

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
        
    	NCMapEditor ed = NmapUiUtils.getActiveNatlCntrsEditor();

    	IDisplayPane[] zoomPanes = ( ed.arePanesGeoSynced() ? 
    								 ed.getDisplayPanes() : ed.getSelectedPanes() );

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