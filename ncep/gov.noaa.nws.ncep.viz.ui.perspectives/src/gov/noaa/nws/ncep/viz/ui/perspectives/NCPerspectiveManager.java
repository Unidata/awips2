package gov.noaa.nws.ncep.viz.ui.perspectives;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.resources.manager.NmapResourceUtils;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.tools.imageProperties.FadeDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
//import gov.noaa.nws.ncep.viz.ui.locator.LocatorDisplay;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.swt.SWT;

import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.viz.application.ProgramArguments;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.ResourceList;

import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.viz.rsc.satellite.units.NcSatelliteUnits;

import gov.noaa.nws.ncep.viz.tools.frame.FrameDataDisplay;

/**
 * Manages the life cycle of the National Centers Perspectives
 * 
 * Installs a perspective watcher that handles the transitions in and out of the
 * National Centers perspectives.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/2008		22			M. Li		Created
 * 03/2009      75          B. Hebbard  Rename class and all references NMAP->NC
 * 08/05/09                 G. Hull     Load a default RBD
 * 09/27/09     #169        G. Hull     create an NCMapEditor and remove non NC editors
 * 11/05/09     183         Q. Zhou     Added Fading scale
 * 11/13/09     180         G. Hull     NmapCommon.NatlCntrsPerspectiveID
 * 02/20/10     226         G. Hull     Use RbdBundle
 * 03/16/10   238, 239      Archana    Added FrameDataDisplay to the status bar.
 * 05/23/10   dr11 migration G. Hull   manage Cave's TimeDisplay 
 * 05/26/10                 G. Hull     Call NcSatelliteUnits
 * 08/27/10     #303        G. Hull    Set the editor name based on the default RBD name 
 * 09/23/10     #307        G. Hull    Load spf from the command line.
 * 10/20/10     #307        G. Hull    NcAutoUpdater
 * 03/22/11   r1g2-9        G. Hull    extend AbstractCAVEPerspectiveManager
 * 06/07/11     #445        X. Guo     Data Manager Performance Improvements
 *                                     Initialize Data resources
 * 
 * </pre>
 * 
 * @author 
 * @version 1.0
 */

public class NCPerspectiveManager extends AbstractCAVEPerspectiveManager {
    /** The National Centers Perspective Class */
	// put this in common to avoid dependencies on this project
    public static final String NC_PERSPECTIVE = NmapCommon.NatlCntrsPerspectiveID;
           
    private int currentRscIndex = 0;

	@Override
	protected void open() {
		
		
        ProductAlertObserver.addObserver(null, new NcAutoUpdater());

        // NatlCntrs uses a different equation to compute the Temperature values from
        // a Satellite IR image so this will override the 'IRPixel' label used by satellite
        // images and will create our Units and UnitConverter class to do the conversion.
        // 
        NcSatelliteUnits.register();
        
        // Load either the default RBD or RBDs in the command line spf
        //
        File rbdFiles[] = new File[1];
        rbdFiles[0] = LocalizationManager.getInstance().getLocalizationFile("defaultRBDFile");

        String spfName = ProgramArguments.getInstance().getString("-spf");

        if( spfName != null && !spfName.isEmpty() ) {

        	// the name of the spf should include a group name
        	// TODO : check that there is a group and if not use a default.
        	if( spfName.indexOf(File.separator) < 0 ) {
        		System.out.println("The -spf argument is specified without an spf group.");
        		// load the default rbd...
        	}
        	else {
        		File spfDir = new File( NmapResourceUtils.getSpfGroupsDir()+File.separator+spfName );
        		ArrayList<RbdBundle> rbdLoadSels = new ArrayList<RbdBundle>();

        		if( !spfDir.exists() || !spfDir.isDirectory() ) {
        			System.out.println("The -spf argument is specified with an unknown spf: "+ spfName );
        			// load the default rbd...
        		}
        		else {
        			File tmpFiles[] = spfDir.listFiles(
        					NmapCommon.createFileFilter( new String[]{".xml"} ));	
        			if( tmpFiles == null || tmpFiles.length == 0 ) {
        				System.out.println("The -spf argument is specified with an empty spf: "+ spfName );
            			// load the default rbd...
        			}
        			else { 
        				rbdFiles = tmpFiles;
        			}
        			// validate the rbds by unmarshalling them??
        			//RbdBundle rbd = RbdBundle.unmarshalRBD( rbdFile, null );
        		}
        	}
        }

        // loop thru the rbds and load them into a new editor.
        for( File rbdFile : rbdFiles ) {
        	try {
        		RbdBundle rbd = RbdBundle.unmarshalRBD( rbdFile, null );
        		
        		NCMapEditor editor = NmapUiUtils.createNatlCntrsEditor( rbd.getRbdName() );
        		rbd.setNcEditor( editor );
        		
        		ResourceBndlLoader rbdLoader = new ResourceBndlLoader( "Loading RBD: "+rbd.getRbdName() );
        		
        		rbdLoader.addRBD( rbd );
        		VizApp.runAsync( rbdLoader );
        	}
        	catch ( Exception ve ) {
        		System.out.println("Could not load rbd: " + ve.getMessage());
        		ve.printStackTrace();
        	}       
        }
//      xguo,06/02/11. To enhance the system performance, move 
//      data resource query to here to initialize Data Resource instance.
        try {
	    	ResourceDefnsMngr rscDefnMngr = ResourceDefnsMngr.getInstance();
	    	rscDefnMngr.generateDynamicResources();
	    } catch( VizException el ) {
			    System.out.println("Could not initialize NC-Data resources: " + el.getMessage());
        		el.printStackTrace();
	    }
	}
	
	@Override
    public void activate() {
		super.activate();
		
// Experiment.
//        statusLine.setErrorMessage("Status Line ERROR MSG B");
//        statusLine.setMessage("Status Line MESSAGE B");

        // relayout the shell since we added widgets
        perspectiveWindow.getShell().layout(true, true);

        NcSatelliteUnits.register();
    }

	@Override
    public void deactivate() {
		super.deactivate();
		
        SatelliteUnits.register();
    }
	

    @Override
    protected List<ContributionItem> getStatusLineItems() {
        List<ContributionItem> stsLineDisplays = new ArrayList<ContributionItem>();
// in reverse order
        stsLineDisplays.add( new FadeDisplay() );
        stsLineDisplays.add( FrameDataDisplay.createInstance() );
  //      stsLineDisplays.add( new LocatorDisplay("LocatorDisplay") ); 
        //stsLineDisplays.add( new TimeDisplay() );
        
        return stsLineDisplays;
    }


	@Override
    public void close() {
		super.close();
		
	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.AbstractVizPerspective#getPerspectiveInputHandlers(com.raytheon.viz.ui.editor.AbstractEditor)
     */
    @Override
    public IInputHandler[] getPerspectiveInputHandlers(
            final AbstractEditor editor) {
    	// currently only implementing handleMouseWheel which is now done below.
        IInputHandler[] superHandlers = super
                .getPerspectiveInputHandlers(editor);

        // If this is a GLMapEditor from D2D then just return the abstractEditors handlers
        // (this won't last long since the perspective will remove/save off the editors.
        if( !(editor instanceof NCMapEditor ) ) {
        	return superHandlers;
        }
        
        // No-Ops for doubleClick, keyUp/Down, mouseDown, mouseHover and mouseUp
        IInputHandler handler = new InputAdapter() {

        	private boolean isShiftDown = false;
        	
            @Override
            public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            	// Set mouse position
            	Coordinate ll = editor.translateClick(x, y);       	
            	//gov.noaa.nws.ncep.viz.ui.locator.LocatorDisplay/*.getInstance()*/.setPosition(ll); 
            	gov.noaa.nws.ncep.viz.common.CoorBean.getInstance().setCoor(ll);
                return false;
            }

            @Override
            public boolean handleMouseMove(int x, int y) {
            	// AbstractVizPerspective was doing this in its handlers so copy here.
                //for (IDisplayPane pane : editor.getDisplayPanes()) {
                //    pane.setLastMouseX(x);
                //    pane.setLastMouseY(y);
                //}

            	// Set mouse position
            	Coordinate ll = ((NCMapEditor)editor).translateClick( x, y);
            	//gov.noaa.nws.ncep.viz.ui.locator.LocatorDisplay/*.getInstance()*/.setPosition(ll);   
            	gov.noaa.nws.ncep.viz.common.CoorBean.getInstance().setCoor(ll);
                return false;
            }

// R1G2-9 migration
//            Raytheon's wheel zoom now in the PanHandler so we will do the same...
//            
//            // Copied from AbstractVizePerspective but we may want to change 
//            @Override
//            public boolean handleMouseWheel(Event event, int x, int y) {
//            	if( ((NCMapEditor)editor).arePanesGeoSynced() ) {
//            		IDisplayPane[] panes = editor.getDisplayPanes();
//            		for (IDisplayPane pane : panes) {
//            			pane.zoom(event.count, event.x, event.y);
//            		}
//            	}
//            	else {
//// Which one of these makes more sense?            		
////            		IDisplayPane pane = editor.getActiveDisplayPane();
//
//            		
//            		IDisplayPane pane = ((NCMapEditor)editor).getCurrentMouseHoverPane();
//            		pane.zoom( event.count, event.x, event.y );            		
//            	}
//
////            	IExtent extent = editor.getActiveDisplayPane().getRenderableDisplay().getExtent();  
//                
////            	IDescriptor descriptor = editor.getActiveDisplayPane().getDescriptor(); 
//            	
////            	DisplayViewLowerLeftAndUpperRightLongLatValues longLatValuesInstance = 
////            		DisplayViewLowerLeftAndUpperRightLongLatValues.getInstance(); 
////            	longLatValuesInstance.initialization(extent, descriptor); 
//
//            	return false;
//            }
//            
            /*
             * (non-Javadoc)
             * 
             * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
             * int, int)
             */
            @Override
            public boolean handleMouseDown(int x, int y, int mouseButton) {

            	if ( mouseButton != 1 ) return false;
            	
            	IDescriptor descriptor = editor.getActiveDisplayPane()
            	.getDescriptor();

            	/*
            	 * get all resource labels
            	LegendEntry[] labels = editor.getActiveDisplayPane()
            	.getRenderableDisplay().getLegendDecorator()
            	.getLegendData(descriptor);
            	if (labels == null || labels.length == 0) {
            		return false;
            	}
            	 */

            	/*
            	 * if click on resource label, toggle its visibility on/off
            	//ResourcePair rsc = editor.getActiveDisplayPane()
            	//.getTarget().checkLabelSpace(labels, x, y);
                IDisplayPane activePane = editor.getActiveDisplayPane();
                IRenderableDisplay display = editor.getActiveDisplayPane()
                        .getRenderableDisplay();
                ILegendDecorator ld = display.getLegendDecorator();
                ResourcePair rsc = ld.checkLabelSpace(descriptor, activePane.getTarget(), x, y);
            	
            	if (rsc != null) {
            		toggleVisibility(rsc);
            		editor.refresh();
            	}
            	 */

            	
            	return false;
            }
            
        	@Override
        	public boolean handleKeyDown(int keyCode) {

        		if ( keyCode == SWT.SHIFT ) {
        			isShiftDown = true;
        			return false;
        		}

        		if ( (keyCode==SWT.ARROW_UP) || (keyCode==SWT.ARROW_DOWN) ) {

        			if ( isShiftDown ) {
        				/*
        				 * Make all resources visible
        				 */
        				ResourceList rl = editor.getActiveDisplayPane().getDescriptor().getResourceList();
        				for (int i=0; i < rl.size(); i++ ) {
        					rl.get(i).getProperties().setVisible(true);
        				}

        			}
        			else {

        				ResourceList rl = editor.getActiveDisplayPane().getDescriptor().getResourceList();

        				int incr = 1;
        				if (keyCode==SWT.ARROW_DOWN) incr = -1;

        				/*
        				 * look for next non map layer resource
        				 */
        				int search = currentRscIndex;
        				do {
        					search += incr;
        					if ( search < 0 ) search = rl.size() - 1;
        					if ( search >= rl.size() ) search = 0;
        					if ( ! rl.get(search).getProperties().isMapLayer() ) {
        						currentRscIndex = search;
        						break;
        					}
        				} while ( search != currentRscIndex );

        				/*
        				 * turn off all non map layer resources
        				 */
        				for (int i=0; i < rl.size(); i++ ) {
        					if ( rl.get(i).getProperties().isMapLayer() )
        						rl.get(i).getProperties().setVisible(true);
        					else
        						rl.get(i).getProperties().setVisible(false);
        				}

        				//  re-enable selected resource.
        				rl.get(currentRscIndex).getProperties().setVisible(true);

        			}
        			editor.refresh();

        		}

        		return false;
        	}
        	
        	@Override
        	public boolean handleKeyUp(int keyCode) {
        		if ( keyCode == SWT.SHIFT ) {
        			isShiftDown = false;
        		}
        		return false;
        	}
            
            private void toggleVisibility(ResourcePair rp) {
            	AbstractVizResource<?, ?> rsc = rp.getResource();
            	if (rsc != null) {
            		rp.getProperties().setVisible(!rp.getProperties().isVisible());
            	}
            }
            
        };

        ArrayList<IInputHandler> handlers = new ArrayList<IInputHandler>();
//        handlers.addAll(Arrays.asList(superHandlers));
        handlers.add(handler);
        return handlers.toArray(new IInputHandler[handlers.size()]);
    }

}
