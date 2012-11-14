package gov.noaa.nws.ncep.viz.tools.predefinedArea;

import gov.noaa.nws.ncep.viz.resources.manager.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.tools.panZoom.ZoomUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.viz.core.CorePlugin;

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
    	try{
    	    areaName = event.getParameter("areaName");
    	} catch (Exception e) {
            MessageBox mb = new MessageBox(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), SWT.OK);
            	mb.setText("PredefinedArea Not Available");
            	mb.setMessage("No description code is available for the requested area.");
            	mb.open();
    	}
    	setGeographicArea(areaName);

        return null;
    }

    /**
     * @param areaName
     */
    public static void setGeographicArea(String areaName) {
        NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
        // get the panes to set the area in. 
        NCDisplayPane[] displayPanes = (NCDisplayPane[]) (editor
                .arePanesGeoSynced() ? editor.getDisplayPanes() : editor
                .getSelectedPanes());
        
        try {
            PredefinedArea pArea = PredefinedAreasMngr
                    .getPredefinedArea(areaName);
            
        	NCMapRenderableDisplay dispPane = pArea.getPredefinedArea();
					
        	for( IDisplayPane pane : displayPanes ) {
                NCMapRenderableDisplay existingDisplay = (NCMapRenderableDisplay) pane
                        .getRenderableDisplay();
                NCMapDescriptor existingMD = (NCMapDescriptor) existingDisplay
                        .getDescriptor();

                // Note: setGridGeometry does an implicit reproject of all
                // resources
        		// on the descriptor, so don't need to do this explicitly
                existingMD.setGridGeometry(dispPane.getDescriptor()
                        .getGridGeometry());
                
                existingDisplay.setZoomLevel(dispPane.getZoomLevel());
                existingDisplay.setMapCenter(dispPane.getMapCenter());
                existingDisplay.setPredefinedAreaName(dispPane
                        .getPredefinedAreaName());
                
                pane.setZoomLevel( dispPane.getZoomLevel());
                pane.scaleToClientArea();
                existingDisplay.recenter(dispPane.getMapCenter());
                existingDisplay.getView().zoom( dispPane.getZoomLevel());
                
                existingMD.setSuspendZoom(false);
                ZoomUtil.allowZoom(editor);
                VizGlobalsManager.getCurrentInstance().updateUI(editor);

        	}
        	editor.refresh();        

        } catch (Exception e) {

            Status status = new Status(Status.ERROR, "com.raytheon.viz.ui", 0,
                    "Error occurred during bundle load.", e);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", "Error occurred during bundle load.", status);
            CorePlugin
                    .getDefault()
                    .getLog()
                    .log(new Status(IStatus.ERROR, CorePlugin.PLUGIN_NAME,
                            "Error occurred during bundle load", e));
        }
    }
}