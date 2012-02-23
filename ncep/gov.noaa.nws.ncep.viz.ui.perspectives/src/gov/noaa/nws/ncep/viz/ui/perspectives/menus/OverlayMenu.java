package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.UiPlugin;

/**
 * Create the Menu Items for the Overlays menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  4/15/11                  G. Hull      created.
 * 12/06/11                  B. Hebbard   sort menu entries alphabetically
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class OverlayMenu extends ContributionItem {
	
	@Override
	public void fill(Menu menu, int index) {

		try {
			List<String> overlayRscTypes = ResourceDefnsMngr.getInstance().getResourceTypesForCategory(
					ResourceName.OverlayRscCategory, "", false );
			Collections.sort(overlayRscTypes, new Comparator<String>() { // alphabetize menu...
					public int compare (String s1, String s2) { // ...case insensitive
						return s1.compareToIgnoreCase(s2);
					}
			});
			
			int ovlyIndx=0;
			for( String overlayRsc : overlayRscTypes ) {		
				if( overlayRsc.equals( NmapCommon.getBaseOverlay() ) ) { 
					continue;
				}
				
				MenuItem ovrlyMenuItem = new MenuItem( menu, SWT.PUSH, ovlyIndx++ );
				ovrlyMenuItem.setText( overlayRsc );
				ovrlyMenuItem.setData( overlayRsc );

				ovrlyMenuItem.addSelectionListener(new SelectionAdapter() {
					public void widgetSelected(SelectionEvent e) {
						loadOverlay( ((MenuItem)e.widget).getText() );
					}
				});
			}
		} catch (VizException e) {
			return;
		}
	}

	private void loadOverlay( final String overlayName ) {
        final NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
        if( editor == null )
            return;
        
		System.out.println("Loading  overlay from menu "+ overlayName );

        Job j = new Job("Loading Map Overlays...") {
            @SuppressWarnings("unchecked")
			@Override
            protected IStatus run(IProgressMonitor monitor) {
                long t0 = System.currentTimeMillis();

                if (overlayName == null) {
                    return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                            "overlayName was null");
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
            		AbstractVizResource ovrlyRsc = rscPair.getResource(); 
            		
                    IDisplayPane[] seldPanes = ((NCMapEditor)editor).getSelectedPanes();
                    
                    if( seldPanes.length == 0 ) {
                    	System.out.println("There are no Selected Panes to load to?");
                    }

                    // this assumes a map bundle has only a single display 
                    for (IDisplayPane pane : seldPanes ) {
                    	existingMD = pane.getRenderableDisplay().getDescriptor();

                    	if(ovrlyRsc == null) {
                    		AbstractResourceData resourceData = rscPair.getResourceData(); 
                    		if(resourceData != null)
                    			ovrlyRsc = resourceData.construct(
                    					rscPair.getLoadProperties(), existingMD); 
                    	}
                    	ResourceList resourceList = existingMD.getResourceList(); 
                    	resourceList.add(ovrlyRsc, props); 
                    	ovrlyRsc.setDescriptor(existingMD);
                    	ovrlyRsc.init( pane.getTarget() );
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
	}
}
