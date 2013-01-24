package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import java.lang.reflect.Constructor;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.UiPlugin;

/**
 *  Resource Bundle Loader will load RBDs into new or existing map editors.
 *  (Note that this was originally designed to be in a separate thread but there was a problem
 *  with the thread loading the RBDs.)
 * 
 *  TODO : This contains commented out code which will implement a Load Mode option that allows the user to either Append
 * RBD resources to a display or to overwrite/replace resources already displayed.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/22/09       99        Greg Hull     created 
 * 08/29/09	     148	    Greg Hull     getFrameTimes()
 * 09/17/09      169        Greg Hull     add multi-panel
 * 10/19/09      169        Greg Hull     add seldTimeline and animMode but not implemented
 * 10/20/09      145        Greg Hull     load with the selected frame times.
 * 10/21/10		#314		Q. Zhou   	  set Hide/Show status for editor
 * 10/22/10      307        Greg Hull     set to first frame for fcst data.
 * 10/29/10      307        Greg Hull     wait for DataCube to init
 * 06/07/11     #445        Xilin Guo     Data Manager Performance Improvements
 * 10/15/11		?			B. Yin		  Keep PGEN resource when loading RBDs.
 * 01/09/12     #561        Greg Hull     rm code to add Locator Resource
 * 02/15/12     #627        Archana       Created the private class RbdBundleEditorWrapper,
 *                                        changed the seldRBDs queue to accept a RbdBundleEditorWrapper
 *                                        and updated the addRBD() to take a NCMapEditor as an additional
 *                                        argument.
 * 04/24/12     #629        B. Hebbard    [TTR 356] if loadSelectedPaneOnly, don't clear out
 *                                        other panes
 * 11/25/12     #630        Greg Hull     Resource defined areas.
 *
 * </pre>
 * 
 * @version 1
 */
public class ResourceBndlLoader implements Runnable {  // extends Job {

	private final class RbdBundleEditorWrapper<RbdBundle, NCMapEditor>{
		private RbdBundle rbdBundle;
		private NCMapEditor mapEditor;
		RbdBundleEditorWrapper(RbdBundle theRbdBundle, NCMapEditor theMapEditor){
			setRbdBundle( theRbdBundle);
			setMapEditor( theMapEditor );
		}
		
		/**
		 * @param rbdBundle the rbdBundle to set
		 */
		public void setRbdBundle(RbdBundle rbdBundle) {
			this.rbdBundle = rbdBundle;
		}
		
		/**
		 * @return the rbdBundle
		 */
		public RbdBundle getRbdBundle() {
			return rbdBundle;
		}
		/**
		 * @param mapEditor the mapEditor to set
		 */
		public void setMapEditor(NCMapEditor mapEditor) {
			this.mapEditor = mapEditor;
		}
		/**
		 * @return the mapEditor
		 */
		public NCMapEditor getMapEditor() {
			return mapEditor;
		}
	}
	
//    private final ConcurrentLinkedQueue<RbdBundle> seldRBDs;
    private final ConcurrentLinkedQueue<RbdBundleEditorWrapper<RbdBundle, NCMapEditor>> seldRBDs;
        
    // when set we will only load the selected Pane
    private boolean loadSelectedPaneOnly = false;
    
    public void setLoadSelectedPaneOnly() {
    	loadSelectedPaneOnly = true;
    }
    
    public void removeAllSeldRBDs() {
    	seldRBDs.clear();
    }

    public void addRBD( RbdBundle newRBD, NCMapEditor theEditor ) {

    	seldRBDs.add( new RbdBundleEditorWrapper<RbdBundle,NCMapEditor> ( newRBD, theEditor ) );
    }

    public ResourceBndlLoader( String name ) { // name of the Job
		//super(name);
		seldRBDs = new ConcurrentLinkedQueue<RbdBundleEditorWrapper<RbdBundle, NCMapEditor>>();
	}
        
    private class ErrorMsg implements Runnable {
    	String errMsg="";
    	ErrorMsg( String em ) {
    		errMsg = em;
    	}
		public void run() {
			Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0, errMsg, null );
			ErrorDialog.openError(Display.getCurrent().getActiveShell(),
					"ERROR", errMsg, status);
		}
	}
    
    // check that the 
    public boolean areSeldRBDsGeoSyncCompatible() {
    	return true;
    }
    
//    @Override
//    protected IStatus run(IProgressMonitor monitor) {
    //public void loadRBDs() {
    public void run() {
    	RbdBundleEditorWrapper[] wrapperClassArray= ( RbdBundleEditorWrapper[] ) seldRBDs.toArray ( new RbdBundleEditorWrapper[0] );
		
    	if( loadSelectedPaneOnly ) {
    		if( wrapperClassArray.length > 1 ) {
    			System.out.println("Warning: rbdLoader should only load one RBD when"+
    					"loadSelectedPaneOnly is true??" );
    		}
    	}
    
		for ( RbdBundleEditorWrapper<RbdBundle,NCMapEditor> thisWrapper : wrapperClassArray ) {
        	
			try {
     		 RbdBundle rbdBndl = thisWrapper.getRbdBundle();
    		
        		// the editor should have already be created with the right 
        		// number of displays and matching paneLayouts 
        		NCMapEditor editor = thisWrapper.getMapEditor();
        		
        		if( editor == null ) {
        			throw new VizException("??editor is null in rbdLoader?");
        		}
        		
        		// If this editor currently has resources loaded, clear them out except for PGEN
        		//
        		for( int r=0 ; r<rbdBndl.getPaneLayout().getRows() ; r++ ) {
        			for( int c=0 ; c<rbdBndl.getPaneLayout().getColumns() ; c++ ) {
        		    	PaneID paneid = new PaneID(r,c);
        		    	IDisplayPane pane = editor.getDisplayPane( paneid );
        		    	
        				if( pane == null ) {
        					throw new VizException("Could not get pane "+paneid.toString() +
        							" from the editor.");
        				}

        		    	// don't clear this pane if we are only loading the selected pane and 
        		    	// this isn't it
        		    	if( loadSelectedPaneOnly &&
        		    		rbdBndl.getSelectedPaneId().compare( paneid ) != 0 ) {
        		    		continue;
        		    	}
        		    	
        				List<ResourcePair> rlist = ((NCDisplayPane)pane).getRenderableDisplay().getDescriptor().getResourceList();
        				if( rlist == null ) {
        					throw new VizException("The ResourceList is empty?");
        				}
        				Iterator<ResourcePair> it = rlist.iterator();
        				
        				while( it.hasNext() ){
        					ResourcePair rp = it.next();
        					if ( !rp.getResource().getClass().getName().endsWith("PgenResource") ) {
        						rlist.remove(rp);
        					}
        				}
        			}
        		}
        		
        		editor.setAutoUpdate(rbdBndl.isAutoUpdate() );
        		editor.setGeoSyncPanesEnabled( rbdBndl.isGeoSyncedPanes() );
        		editor.setHideShow(false); //init to false, means rsc on
        		
        		IDisplayPane displayPanes[] = editor.getDisplayPanes();
        		IDisplayPane seldPane = null;
        		
        		if( loadSelectedPaneOnly ) {
        			
        			if( editor.getPaneLayout().getRows() <= rbdBndl.getSelectedPaneId().getRow() ||
        				editor.getPaneLayout().getColumns() <= rbdBndl.getSelectedPaneId().getColumn() ) {
        				
        				throw new VizException("Error: The Active Display doesn't have enough Panes"+
    					" for the selected Pane: ");
//        				break;
        			}
        		}
        		else if( !editor.getPaneLayout().equals( rbdBndl.getPaneLayout() ) ) {
        			throw new VizException("PaneLayouts of the RBD and Editor don't match?");
        		}
        		
        		// loop thru the panes in the RBD
        		//
        		for( int r=0 ; r<rbdBndl.getPaneLayout().getRows() ; r++ ) {
        		    for( int c=0 ; c<rbdBndl.getPaneLayout().getColumns() ; c++ ) {
        		    	PaneID paneid = new PaneID(r,c);
        		    	
        		    	// don't load this pane if we are only loading the selected pane and 
        		    	// this isn't it
        		    	if( loadSelectedPaneOnly &&
        		    		rbdBndl.getSelectedPaneId().compare( paneid ) != 0 ) {
        		    		continue;
        		    	}

        		    	IDisplayPane displayPane = editor.getDisplayPane( paneid ); 
        		    	AbstractRenderableDisplay mapDisp = rbdBndl.getDisplayPane( paneid );

        		    	if( seldPane == null ) {
        		    		seldPane = displayPane;
        		    	}
        		    	
        		    	// if the editor was just created and there was an error, close the editor.
        		    	// TODO: if there is an error, prompt if the user wishes to continue.
        		    	if( loadResourceBundleDefn( displayPane, 
        		    			(NCMapRenderableDisplay) mapDisp, 
        		    			rbdBndl.getTimeMatcher() ) == false ) {

        		    		throw new VizException("Error Loading Pane "+paneid.toString()+
        		    				" for RBD "+ rbdBndl.getRbdName() );
        		    	}
        		    }
        		}

        		editor.refresh();
        		editor.refreshGUIElements();

        		NmapUiUtils.bringToTop( editor );        		
        	}
			catch ( VizException vizex ) {
	    		VizApp.runAsync(
	    				new ErrorMsg( vizex.getMessage() ) );	
			}        		
    	}
    	
		removeAllSeldRBDs();
    	
     //   this.cancel();  if a Job 
     //   return null;
    }
    	
    //
    //
    public boolean loadResourceBundleDefn( IDisplayPane pane, 
                            NCMapRenderableDisplay mapDisplay, NCTimeMatcher timeMatcher )  {

    	if( timeMatcher == null ) {
    		System.out.println("Error Loading Timeline???");
    		return false;
    	}
    	
    	MapDescriptor descr = (MapDescriptor)mapDisplay.getDescriptor();

    	descr.setTimeMatcher( timeMatcher ); 
    	descr.setNumberOfFrames( timeMatcher.getNumFrames() );
    	DataTime[] dataTimes = timeMatcher.getFrameTimes().toArray( new DataTime[0] );

    	if( dataTimes == null || 
    		dataTimes.length == 0 ) {
 //   		descr.setDataTimes( null );    		
    	}
    	else {
    		descr.setDataTimes( dataTimes );
    		
    		if( timeMatcher.isForecast() ) {
    			descr.setFrame( 0 );
    		}
    		else {
    			descr.setFrame( dataTimes.length-1 );
    		}
    	}

    	ResourceList rscList = descr.getResourceList();
		   	
		//Add PGEN resource back
		if ( !pane.getRenderableDisplay().getDescriptor().getResourceList().isEmpty() ){
			rscList.addAll( pane.getRenderableDisplay().getDescriptor().getResourceList());
		}

    	rscList.instantiateResources( descr, true );

    	// TODO : change the resource-capable (ie Satellite) resourceData objects to be able
    	// to query for the gridCoverage before the resource object is created. We should
    	// be able to query the mcidas_area_names/mcidas_spatial tables knowing just the info
    	// in the resourceData object.
    	//       But until then resourceData objects just return dummy coverages and 
    	// we will need to reproject the mapDisplay after the resource is loaded
    	// if the area is to be set from a resource-defined area, we need
    	//    	
    	pane.setRenderableDisplay( mapDisplay );
    	
//    	NCDisplayPane ncPane = (NCDisplayPane)pane;
    	
    	PredefinedArea initArea = mapDisplay.getInitialArea();
    	
    	if( initArea.getAreaSource() == AreaSource.RESOURCE_DEFINED ) {
    		
    		if( initArea.getZoomLevel().equals( ZoomLevelStrings.SizeOfImage.toString() ) ) {
    			
    	        mapDisplay.setExtent( new PixelExtent( pane.getBounds() ) );
    	        ((NCMapDescriptor)descr).setSuspendZoom( true );
//    	        ZoomUtil.suspendZoom( mapDisplay.getContainer() ) ;    		
    		}
    		else if( initArea.getZoomLevel().equals( ZoomLevelStrings.FitToScreen.toString() ) ) {
    			
    		}
    	}
    	
    	pane.setZoomLevel( mapDisplay.getZoomLevel() );
    	pane.resize();
    	pane.refresh();

    	return true;
    }    
}