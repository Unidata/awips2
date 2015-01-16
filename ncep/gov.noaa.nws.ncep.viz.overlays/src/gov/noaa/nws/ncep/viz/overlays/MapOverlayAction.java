package gov.noaa.nws.ncep.viz.overlays;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceCategory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

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
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.uf.viz.core.maps.MapStore;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;


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
        final AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
        if( editor == null )
            return null;

        Job j = new Job("Loading Overlay...") {
            @SuppressWarnings("unchecked")
			@Override
            protected IStatus run(IProgressMonitor monitor) {
                long t0 = System.currentTimeMillis();
                String overlayName = arg0.getParameter("overlayName");
                //System.out.println(overlayName);
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
                			ResourceCategory.OverlayRscCategory, overlayName, null );
                	
                	ResourceSelection rbt = ResourceFactory.createResource( fullRscName ); 
                	ResourcePair rscPair = rbt.getResourcePair();
            		ResourceProperties props = rscPair.getProperties();
            		AbstractResourceData ovrlyRscData = rscPair.getResourceData(); 
            		AbstractVizResource ovrlyRsc = rscPair.getResource(); 
                    IDisplayPane[] seldPanes = NcEditorUtil.getSelectedPanes(editor);
                    
                    if( seldPanes.length == 0 ) {
                    	System.out.println("There are no Selected Panes to load to?");
                    }
                    
                            
                    // this assumes a map bundle has only a single display 
                    for (IDisplayPane pane : seldPanes ) {
                    	
                    	existingMD = pane.getRenderableDisplay().getDescriptor();
                    	ResourceList resourceList = existingMD.getResourceList();
                    	

                    	ResourcePair rp = new ResourcePair();
                    	rp.setResourceData( ovrlyRscData );
						
                    	for (ResourcePair rpe : resourceList) {
                    		// If resource is already loaded
                            if (rpe.getResource() != null && rpe.getResource().getName() != null
                                    && rpe.getResourceData().equals(ovrlyRscData)) {
                            		//rpe.setResourceData( rpe.getResourceData() );
                            		rp.setResourceData( null );
                            		
                            		resourceList.remove( rpe ); 
                            		//resourceList.removeRsc(rp.getResource());
                            		break;
                            		//unload map from the current mapDescriptor
                                    //AbstractVizResource<?, ?> rsc = map.getResource();
                                    //mapDescriptor.getResourceList().removeRsc(rsc);
                            } else {
                            	//ResourcePair rp = new ResourcePair();
                            	//rp.setResourceData( ovrlyRscData );
                            	
                            	
                            	resourceList.add( rp ); 
                            }	
                        }
                    	
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
        //NcEditorUtil.refreshGUIElements(NcDisplayMngr.getActiveNatlCntrsEditor());
        // this doesn't appear to change anything
        //NcEditorUtil.refreshGUIElements( (AbstractEditor)editor );
        return null;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.
     * menus.UIElement, java.util.Map)
     */
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
    	
    	String ResourceOverlayName = (String) parameters.get("overlayName");
    	    	
    	ResourceName fullRscName = new ResourceName( 
    			ResourceCategory.OverlayRscCategory, ResourceOverlayName, null );
    	
    	AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	if (editor == null) {
            return;
        }
    	
    	System.out.println("---------");
    	
        IDescriptor descriptor = editor.getActiveDisplayPane().getDescriptor();
        
        if (descriptor instanceof IMapDescriptor) {

        	for (ResourcePair rscName : descriptor.getResourceList() ) {
        		
        		//if ( rscName.getResource().getName() != null ) {
        			
        			System.out.println("fullRscName.getRscType(): " + fullRscName.getRscType());
        			System.out.println("ResourceOverlayName: " + ResourceOverlayName);
        			//System.out.println("rscName.getResource().getName(): " + rscName.getResource().getName());
        			if ( rscName.getResource().getName() == ResourceOverlayName) {
        				element.setChecked( true );
        			}
        			
        		//}
        		
        	}
        }
    }
}