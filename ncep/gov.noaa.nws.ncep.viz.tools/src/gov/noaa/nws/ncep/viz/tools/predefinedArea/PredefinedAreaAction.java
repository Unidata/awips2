package gov.noaa.nws.ncep.viz.tools.predefinedArea;

import java.util.Iterator;

import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
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
 * May 15  2013    #862     G. Hull      replace code to get areas from displays or resources with call to 
 *                                       new NcAreaProviderMngr.createGeomProvider() with areaSource & name 
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

        AreaName areaName = null;
        
        try {
        	try {
        		areaName = new AreaName( 
        				AreaSource.getAreaSource( event.getParameter("areaSource") ),
        										  event.getParameter("areaName") );
        	} catch (Exception e) {
        		throw new VizException("areaName or areaSource parameter not recognized");
        	}

        	AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
        	NcDisplayType dt = NcEditorUtil.getNcDisplayType( editor );
        	if( dt != NcDisplayType.NMAP_DISPLAY ) {
        		return null;
        	}

        	IGridGeometryProvider geomProv = 
        				NcAreaProviderMngr.createGeomProvider( areaName );

        	if( geomProv == null ) { 
            	throw new VizException("Unable to create area for,"+areaName.toString() );
        	}
        		
        	PredefinedArea pArea = PredefinedAreaFactory.createPredefinedArea( geomProv );

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

        } 
        catch (VizException e) {        	
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

    	// Note: setGridGeometry does an implicit reproject of all
    	// resources
    	// on the descriptor, so don't need to do this explicitly
    	existingDisplay.setInitialArea( pArea );
    	
    	pane.setZoomLevel( existingDisplay.getZoomLevel() );
 
    	pane.scaleToClientArea();
    	existingDisplay.recenter( existingDisplay.getMapCenter() );
    	
    	existingDisplay.getView().zoom( 
    			existingDisplay.getZoomLevel() );

    	((INatlCntrsDescriptor)existingDisplay.getDescriptor()).setSuspendZoom(false);
    }
}