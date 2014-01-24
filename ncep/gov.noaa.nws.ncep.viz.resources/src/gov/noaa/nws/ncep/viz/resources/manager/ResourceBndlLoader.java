package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import org.eclipse.swt.graphics.Rectangle;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;

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
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * 02/01/13     #972        Greg Hull     RbdBundleEditorWrapper doesn't need to be a generic
 * 02/12/13     #972        Greg Hull     NcDisplayType and NatlCntrsEditor
 * 11/26/13     #1078       Greg Hull     Size Of Image fix (PixelExtent constructor)
 *
 * </pre>
 * 
 * @version 1
 */
public class ResourceBndlLoader implements Runnable {  // extends Job {

	private final class RbdBundleEditorWrapper {
		private AbstractRBD<?> rbdBundle;
		private AbstractEditor ncEditor;
		private Boolean replaceEditorNameWithRbdName;
		
		RbdBundleEditorWrapper(AbstractRBD<?> theRbdBundle, AbstractEditor ed,
				Boolean useRBDName ){
			rbdBundle = theRbdBundle;
			ncEditor = ed;
			NcEditorUtil.setDisplayAvailable( ed, false );
			replaceEditorNameWithRbdName = useRBDName;
		}
		
		public AbstractRBD<?> getRbdBundle() {
			return rbdBundle;
		}
		
		public AbstractEditor getNcEditor() {
			return ncEditor;
		}
		
		public Boolean useRbdNameForEditor() {
			return replaceEditorNameWithRbdName;
		}
	}
	
    private final ConcurrentLinkedQueue<RbdBundleEditorWrapper> seldRBDs;
        
    // when set we will only load the selected Pane
    private boolean loadSelectedPaneOnly = false;
    
    public void setLoadSelectedPaneOnly() {
    	loadSelectedPaneOnly = true;
    }
    
    public void removeAllSeldRBDs() {
    	seldRBDs.clear();
    }
    
    public void addDefaultRBD( NcDisplayType dt, AbstractEditor theEditor ) throws VizException {
		AbstractRBD<?> rbd = NcMapRBD.getDefaultRBD( NcDisplayType.NMAP_DISPLAY );
		
		rbd.resolveLatestCycleTimes(); // shouldn't be needed  but just in case		
    	seldRBDs.add( new RbdBundleEditorWrapper( rbd, theEditor, false ) );    	
    }

    public void addRBD( AbstractRBD<?> newRBD, AbstractEditor theEditor ) {
    	
    	seldRBDs.add( new RbdBundleEditorWrapper( newRBD, theEditor, true ) );
    }

    public ResourceBndlLoader( String name ) { // name of the Job
		//super(name);
		seldRBDs = new ConcurrentLinkedQueue<RbdBundleEditorWrapper>();
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
    
		for ( RbdBundleEditorWrapper thisWrapper : wrapperClassArray ) {        	

			try {
				AbstractRBD<?> rbdBndl = thisWrapper.getRbdBundle();
    		        		
        		// the editor should have already be created with the right 
        		// number of displays and matching paneLayouts 
				AbstractEditor editor = thisWrapper.getNcEditor();
        		
        		if( editor == null ) {
        			throw new VizException("??editor is null in rbdLoader?");
        		}
        		
        		// If this editor currently has resources loaded, clear them out except for PGEN
        		//
        		for( int paneIndx=0 ; paneIndx<rbdBndl.getPaneLayout().getNumberOfPanes() ; paneIndx++ ) {
    				NcPaneID paneid = (NcPaneID) rbdBndl.getPaneLayout().createPaneId(paneIndx);
    				IDisplayPane pane = NcEditorUtil.getDisplayPane( editor, paneid );
    		    	
    				if( pane == null ) {
    					throw new VizException("Could not get pane "+paneid.toString() +
    							" from the editor.");
    				}

    		    	// don't clear this pane if we are only loading the selected pane and 
    		    	// this isn't it
    		    	if( loadSelectedPaneOnly &&
    		    		rbdBndl.getSelectedPaneId().compareTo( paneid ) != 0 ) {
    		    		continue;
    		    	}
    		    	
    				List<ResourcePair> rlist = pane.getRenderableDisplay().getDescriptor().getResourceList();
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
        		
        		NcEditorUtil.setAutoUpdate( editor, rbdBndl.isAutoUpdate() );
        		NcEditorUtil.setGeoSyncPanesEnabled( editor, rbdBndl.isGeoSyncedPanes() );
        		NcEditorUtil.setHideShow( editor, false); //init to false, means rsc on
        		
        		IDisplayPane displayPanes[] = editor.getDisplayPanes();
        		IDisplayPane seldPane = null;
        		INcPaneLayout playout = NcEditorUtil.getPaneLayout( editor );
        		
        		if( loadSelectedPaneOnly ) {
        			
        			if( !playout.containsPaneId( rbdBndl.getSelectedPaneId() ) ) {
//        					playout.getRows() <= rbdBndl.getSelectedPaneId().getRow() ||
//        				playout.getColumns() <= rbdBndl.getSelectedPaneId().getColumn() ) {
//        				
        				throw new VizException("Error: The Active Display doesn't have enough Panes"+
    						" for the selected Pane: ");
        			}
        		}
        		else if( !playout.equals( rbdBndl.getPaneLayout() ) ) {
        			throw new VizException("PaneLayouts of the RBD and Editor don't match?");
        		}
        		
        		// loop thru the panes in the RBD
        		//
        		for( int paneIndx=0 ; paneIndx<rbdBndl.getPaneLayout().getNumberOfPanes() ; paneIndx++ ) {
        			NcPaneID paneid = (NcPaneID)rbdBndl.getPaneLayout().createPaneId( paneIndx );

        			// don't load this pane if we are only loading the selected pane and 
        			// this isn't it
        			if( loadSelectedPaneOnly &&
        				rbdBndl.getSelectedPaneId().compareTo( paneid ) != 0 ) {
        				continue;
        			}

        			IDisplayPane displayPane = NcEditorUtil.getDisplayPane( editor, paneid ); 
        			INatlCntrsRenderableDisplay mapDisp = rbdBndl.getDisplayPane(  paneid );

        			if( seldPane == null ) {
        				seldPane = displayPane;
        			}

        			// if the editor was just created and there was an error, close the editor.
        			// TODO: if there is an error, prompt if the user wishes to continue.
        			if( loadResourceBundleDefn( displayPane, 
        					mapDisp, rbdBndl.getTimeMatcher() ) == false ) {

        				throw new VizException("Error Loading Pane "+paneid.toString()+
        						" for RBD "+ rbdBndl.getRbdName() );
        			}        		    	
        		}
        		
        		// if using the RBD name as the display name, set the tab title
        		// (this will be the case unless we are loading the default RBD in
        		// which case the name may be different. ie. NCTEXT, NSHARP or user-defined.
        		//
        		if( thisWrapper.useRbdNameForEditor() ) {

        			NcDisplayName dispName = NcEditorUtil.getDisplayName( editor );
        			dispName = new NcDisplayName( dispName.getId(), rbdBndl.getRbdName() );
        			NcEditorUtil.setDisplayName( editor, dispName);
        			
            		editor.setTabTitle( dispName.toString() );
        		}
        		
        		editor.refresh();
        		NcEditorUtil.refreshGUIElements( editor );
        		// flag the editor as 'loaded' so that it is no longer considered 'empty' or available
            	NcEditorUtil.setDisplayAvailable( editor, rbdBndl.getIsDefaultRbd() );

        		NcDisplayMngr.bringToTop( editor );        		
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
    				INatlCntrsRenderableDisplay mapDisplay, NCTimeMatcher timeMatcher )  {

    	if( timeMatcher == null ) {
    		System.out.println("Error Loading  Timeline???");
    		return false;
    	}
    	
    	INatlCntrsDescriptor descr = (INatlCntrsDescriptor) mapDisplay.getDescriptor();

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

		// Add PGEN resource back
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
    	    	
		PredefinedArea initArea = mapDisplay.getInitialArea();

		if( initArea.getSource() != AreaSource.PREDEFINED_AREA ) {

			if( initArea.getZoomLevel().equals( ZoomLevelStrings.SizeOfImage.toString() ) ) {
				Rectangle rect = pane.getBounds();
//				mapDisplay.setExtent( new PixelExtent( rect )  );
				mapDisplay.setExtent( new PixelExtent( 
						rect.x, rect.x + rect.width, 
						rect.y, rect.y + rect.height ) );
				((NCMapDescriptor)descr).setSuspendZoom( true );
//				    	        ZoomUtil.suspendZoom( mapDisplay.getContainer() ) ;    		
			}
			else if( initArea.getZoomLevel().equals( ZoomLevelStrings.FitToScreen.toString() ) ) {

			}
		}
    	
    	pane.setZoomLevel( mapDisplay.getZoomLevel() );
    	pane.refresh();

    	return true;
    }    
}