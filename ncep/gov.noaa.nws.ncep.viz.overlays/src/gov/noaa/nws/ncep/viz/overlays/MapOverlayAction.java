package gov.noaa.nws.ncep.viz.overlays;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.File;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.UiPlugin;


/**
 * 
 * Map Overlay Handler
 * 
 * Loads a map overlay from the Overlays menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 17 Dec 2008  30         B. Hebbard  Created; stolen from LoadMap
 *                                     (on which initial implementation
 *                                     based) but uses new directory and
 *                                     allows future enhancement	
 * 03/23/09     #85        Greg Hull   display using the dflt attributes          
 * 06/12/09     #115       Greg Hull   Use ResourceSelectionUnused class to create the overlay.   
 * 08/11/09                Greg Hull   getOverlayResourcesDir() now has '/'      
 * 09/23/09     #169       Greg Hull   multi-panes    
 * 02/18/10     #226       Greg Hull   new RscBndleTemplate constructor       
 * 08/11/10     #273       Greg Hull   bundleName->overlayName and use ResourceFactory to create 
 *                                     the overlay resource.
 * 09/12/12     #869       Greg Hull   call instantiateResources instead of construct().
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */
public class MapOverlayAction extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */    
    @Override
    public Object execute(final ExecutionEvent arg0) throws ExecutionException {

        if (arg0.getCommand() == null) {
            return null;
        }

        final NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
        if( editor == null )
            return null;

        Job j = new Job("Loading Map Overlays...") {
            @SuppressWarnings("unchecked")
			@Override
            protected IStatus run(IProgressMonitor monitor) {
                long t0 = System.currentTimeMillis();
                String overlayName = arg0.getParameter("overlayName");

                if (overlayName == null) {
                    return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                            "bundleName was null");
                }

                try {
                    IDisplayPane displayPane = editor.getActiveDisplayPane();
                    IDescriptor existingMD = displayPane.getDescriptor();

                	// get the name of the default attr set and create the overlay resource
                	//String qualRscName = NmapCommon.OverlaysRscDir + bundleName;
                	ResourceName fullRscName = new ResourceName( 
                			ResourceName.OverlayRscCategory, overlayName, null );
                	
                	ResourceSelection rbt = ResourceFactory.createResource( fullRscName ); 
                	ResourcePair rscPair = rbt.getResourcePair();
            		ResourceProperties props = rscPair.getProperties();
            		AbstractResourceData ovrlyRscData = rscPair.getResourceData(); 
            		
                    IDisplayPane[] seldPanes = ((NCMapEditor)editor).getSelectedPanes();
                    
                    if( seldPanes.length == 0 ) {
                    	System.out.println("There are no Selected Panes to load to?");
                    }

                    // this assumes a map bundle has only a single display 
                    for (IDisplayPane pane : seldPanes ) {
                    	existingMD = pane.getRenderableDisplay().getDescriptor();

                    	ResourceList resourceList = existingMD.getResourceList(); 
                    	ResourcePair rp = new ResourcePair();
                    	rp.setResourceData( ovrlyRscData );
                    	resourceList.add( rp ); 
                    	resourceList.instantiateResources( existingMD, true );
                    }

                    editor.refresh();
                } catch (VizException e) {
                    return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                            "Error loading bundle", e);
                }
                long t2 = System.currentTimeMillis();
                System.out.println("Total bundle retrieval: " + (t2 - t0));
                return Status.OK_STATUS;
            }
        };

        j.schedule();

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.menus.UIElement,
     *      java.util.Map)
     */
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
        if (editor == null) {
            return;
        }

        IDescriptor descriptor = editor.getActiveDisplayPane().getDescriptor();
        if (descriptor instanceof IMapDescriptor) {
            //element.setChecked(((IMapDescriptor) descriptor).getMapManager()
            //        .isMapLoaded((String) parameters.get("mapName")));
            element.setChecked(MapManager.getInstance((IMapDescriptor) descriptor)
                    .isMapLoaded((String) parameters.get("mapName")));
            
            //MapManager.getInstance((IMapDescriptor) descriptor)
        }
    }
}