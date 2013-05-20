package gov.noaa.nws.ncep.viz.tools.predefinedArea;

import java.util.Iterator;

import gov.noaa.nws.ncep.viz.common.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.display.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.common.display.PredefinedArea.AreaSource;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.core.CorePlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Load the scale bundle and merge it into the existing bundle
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 09, 2009             gilbert      Started with RTS class com.raytheon.uf.viz.d2d.ui.map.actions.ScaleHandler 
 *                                       and added our modifications from TO10
 *                                       version of PredefinedAreaAction
 * Sep 25, 2009             B. Hebbard   Zap the isMapLayer() exemption for copying resources in setScale(),
 *                                       as temporary(?) workaround to allow things to work with our new
 *                                       area bundles which have map overlays removed.  Still need to verify
 *                                       permanent solution which correctly preserves all display resources.
 * Oct 09, 2009             B. Hebbard   Switch design to that proposed by Greg:  No longer copy resources
 *                                       to new map descriptor; rather, just get the new parameters from the
 *                                       bundle, and apply to the existing descriptor and display.  (Position
 *                                       error resolved via recenter() and changing order of method calls.)
 * Oct 10, 2009             G. Hull      Multi-Pane
 * Oct 14, 2009             B. Hebbard   Added proper zooming to the newly selected area
 * Oct 27, 2009             G. Hull      Moved out of perspectives project
 * Feb 26. 2010             G. Hull      retrieve PredefinedArea instead of a Bundle file
 * Sep 10. 2012				B. Yin		 Remove the call to setRenderableDisplay which creates a new GLTarget
 * Nov 18, 2012             G. Hull      add areaType parameter and code to get the area based on other types (ie RESOURCES and DISPLAYS)
 * Dec 12  2012    #630     G. Hull      replace ZoomUtil.allowZoom with refreshGUIelements
 * Feb 12  2012    #972     G. Hull      change to INatlCntrsRenderableDisplay
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PredefinedAreaAction extends AbstractHandler {

    public PredefinedAreaAction() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        String areaName = null;
        String areaType = null;
        
        try {
        	try {
        		areaName = event.getParameter("areaName");
        		areaType = event.getParameter("areaType");
        	} catch (Exception e) {
        		throw new VizException("areaName parameter not set???");
        	}

        	AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();

        	PredefinedArea pArea;

        	if( areaType.equals( AreaSource.PREDEFINED_AREA.toString() ) ) {
        		pArea = PredefinedAreasMngr.getPredefinedArea( 
        				NcEditorUtil.getNcDisplayType( editor ), areaName );
        	}
        	else if( areaType.equals( AreaSource.RESOURCE_DEFINED.toString() ) ) {
                ResourceName rscName = new ResourceName( areaName );
        		pArea = getAreaFromResource(editor, rscName );
        	}
        	else if( areaType.equals( AreaSource.DISPLAY_AREA.toString() ) ) {
        		
        		pArea = getAreaFromDisplayPane( areaName );
        	}
        	else { 
        		throw new VizException("Unknown areaType: "+areaType );
        	}

        	// get the panes to set the area in.
        	IDisplayPane[] displayPanes = (IDisplayPane[])
        			(NcEditorUtil.arePanesGeoSynced(editor) ? editor.getDisplayPanes() : 
        					NcEditorUtil.getSelectedPanes(editor));

        	for( IDisplayPane pane : displayPanes ) {

        		setPredefinedArea( pane, pArea );
        	}

        	NcEditorUtil.refreshGUIElements(editor);

        	VizGlobalsManager.getCurrentInstance().updateUI(editor);

        	editor.refresh();

        } catch (VizException e) {        	
        	MessageDialog errDlg = new MessageDialog( 
        			NcDisplayMngr.getCaveShell(), "Error", null, 
        			"Error Changing Area:\n\n"+e.getMessage(),
        			MessageDialog.ERROR, new String[]{"OK"}, 0);
        	errDlg.open();
        }

        return null;
    }

    /**
     * @param areaName
     * @throws VizException 
     */
    public static void setPredefinedArea( IDisplayPane pane,
    									  PredefinedArea pArea ) throws VizException {

    	INatlCntrsRenderableDisplay existingDisplay = 
    		(INatlCntrsRenderableDisplay) pane.getRenderableDisplay();

//    	if( !(existingDisplay instanceof INatlCntrsRenderableDisplay) ) {
//    		return; // only meaningful for map displays
//    	}
    	
    	// Note: setGridGeometry does an implicit reproject of all
    	// resources
    	// on the descriptor, so don't need to do this explicitly
    	existingDisplay.setInitialArea( pArea );
    	
//    	existingDisplay.setPredefinedArea( pArea );
 
    	pane.setZoomLevel( existingDisplay.getZoomLevel() );
 
    	pane.scaleToClientArea();
    	existingDisplay.recenter( existingDisplay.getMapCenter() );
    	
    	existingDisplay.getView().zoom( 
    			existingDisplay.getZoomLevel() );

    	((INatlCntrsDescriptor)existingDisplay.getDescriptor()).setSuspendZoom(false);
    }

    
    private PredefinedArea getAreaFromResource( 
    		AbstractEditor editor, ResourceName rscName ) throws VizException {

    	IDisplayPane[] displayPanes = (IDisplayPane[])editor.getDisplayPanes();
    	 
    	for( IDisplayPane p : displayPanes ) {

    		ResourceList rlist = p.getDescriptor().getResourceList();
    		Iterator<ResourcePair> iter = rlist.iterator();

    		while( iter.hasNext()) {
    			ResourcePair rp = iter.next();

    			if( rp.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData &&
    					rp.getResourceData() instanceof IGridGeometryProvider ) {
    				ResourceName rName = ((AbstractNatlCntrsRequestableResourceData)rp.getResourceData()).getResourceName();

    				if( rscName.equals( rName ) ) {
    					IGridGeometryProvider gridCovRsc = (IGridGeometryProvider)rp.getResourceData();
//    					if( gridCovRsc.getGridGeometry() != null ) {
    					PredefinedArea pArea = new PredefinedArea( 
    			        		AreaSource.RESOURCE_DEFINED,
    			        		gridCovRsc.getProviderName(), 
    			        		gridCovRsc.getGridGeometry(),
    			        		gridCovRsc.getMapCenter(),
    			        		gridCovRsc.getZoomLevel(),
    			        		((AbstractNatlCntrsRequestableResourceData)rp.getResourceData()).getSupportedDisplayTypes()[0] );

    						return pArea; //.getGridGeometry();
//    					}
    				}
    			}
    		}
    	}
    	throw new VizException( "Unable to change the Display Area. \n"
    				+ "The Resource "+rscName.toString()+" is not loaded in this editor.\n");
    }
    
    
    private PredefinedArea getAreaFromDisplayPane( String paneName ) throws VizException {
    	
    	String displayName = paneName;
    	NcPaneID paneId = new NcPaneID(); // 0,0
    	
    	if( displayName.endsWith(")") ) {
    		paneId = NcPaneID.parsePaneId(
    				displayName.substring( displayName.lastIndexOf('(')+1,
    									   displayName.lastIndexOf(')') ) );

    		displayName = displayName.substring(0, displayName.indexOf('(') );
    	}

    	AbstractEditor ed = NcDisplayMngr.findDisplayByID( 
    			NcDisplayName.parseNcDisplayNameString( displayName ) );
    	PredefinedArea pArea=null;
    	
    	if( ed != null ) {
    		IDisplayPane[] panes = ed.getDisplayPanes();
    		for( IDisplayPane p : panes ) {

    			NCMapRenderableDisplay rdisp = (NCMapRenderableDisplay)p.getRenderableDisplay();

    			if( rdisp.getPaneId().equals( paneId) ) {
    				pArea = rdisp.getCurrentArea();
    				return pArea;
    			}
    		}
    	}
    	
    	throw new VizException("Unable to find Display for: "+paneName );
    }
}