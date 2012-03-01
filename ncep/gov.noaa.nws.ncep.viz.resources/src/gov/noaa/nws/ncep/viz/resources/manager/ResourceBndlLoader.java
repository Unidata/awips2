package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;

import java.lang.reflect.Constructor;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;

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
 * 01/09/11     #561        Greg Hull     rm code to add Locator Resource
 * 
 * </pre>
 * 
 * @version 1
 */
public class ResourceBndlLoader implements Runnable {  // extends Job {
	
    private final ConcurrentLinkedQueue<RbdBundle> seldRBDs;
        
    // when set we will only load the selected Pane
    private boolean loadSelectedPaneOnly = false;
    
    public void setLoadSelectedPaneOnly() {
    	loadSelectedPaneOnly = true;
    }
    
    public void removeAllSeldRBDs() {
    	seldRBDs.clear();
    }

    public void addRBD( RbdBundle newRBD ) {
    	seldRBDs.add( newRBD );
    }
    
    public ResourceBndlLoader( String name ) { // name of the Job
		//super(name);
		seldRBDs = new ConcurrentLinkedQueue<RbdBundle>();
	}
        
    private class ErrorMsg implements Runnable {
    	String errMsg="";
    	ErrorMsg( String em ) {
    		errMsg = em;
    	}
		public void run() {
			ErrorDialog.openError(Display.getCurrent().getActiveShell(),
					"ERROR", errMsg, null);
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

		// wait for the system to init
    	/*
		for( int i=0 ; i<15 ; i++  ) {
			if( DataCubeContainer.isInitFinished() ) {
//				System.out.println("DataCubeContainer isinitialized");
				break;
			}
			try {
//				System.out.println("DataCubeContainer is not initialized yet?");
				Thread.sleep(1000);
			} catch (InterruptedException e) {
			}			
		}
		*/
		
		
    	RbdBundle[] rbdList  = (RbdBundle[])seldRBDs.toArray( new RbdBundle[0] );    
    	
    	if( loadSelectedPaneOnly ) {
    		if( rbdList.length > 1 ) {
    			System.out.println("Warning: rbdLoader should only load one RBD when"+
    					"loadSelectedPaneOnly is true??" );
    		}    		
    	}
    	
    	for( RbdBundle rbdBndl : rbdList ) {    		    		
    		// initialize the timeline if it has not already
    		// been initialized
// xguo,06/02/11. Not initialize time-line until the user selects it
//    		rbdBndl.initTimeline();
    		
    		// the editor should have already be created with the right 
    		// number of displays and matching paneLayouts 
    		NCMapEditor editor = rbdBndl.getNcEditor();
    		
    		if( editor == null ) {
    			System.out.println("??editor is null in rbdLoader?");
    			continue;
    		}
    		
    		// If this editor currently has resources loaded, clear them out except for PGEN
    		for( IDisplayPane pane : editor.getDisplayPanes() ) {    			
    			List<ResourcePair> rlist = ((NCDisplayPane)pane).getRenderableDisplay().getDescriptor().getResourceList();
    			Iterator<ResourcePair> it = rlist.iterator();
    			
    			while( it.hasNext() ){
    				ResourcePair rp = it.next();
    				if ( !rp.getResource().getClass().getName().endsWith("PgenResource") ) {
    					rlist.remove(rp);
    				}
    			}
    		}
    		
    		editor.setGeoSyncPanesEnabled( rbdBndl.isGeoSyncedPanes() );
    		editor.setHideShow(false); //init to false, means rsc on
    		    		    		    		
    		IDisplayPane displayPanes[] = editor.getDisplayPanes();
    		
    		if( loadSelectedPaneOnly ) {
    			
    			if( editor.getPaneLayout().getRows() <= rbdBndl.getSelectedPaneId().getRow() ||
    				editor.getPaneLayout().getColumns() <= rbdBndl.getSelectedPaneId().getColumn() ) {
    				
    				System.out.println("Error: The Active Display doesn't have enough Panes"+
					" for the selected Pane: ");    				
    				break;
    			}
    		}
    		else if( !editor.getPaneLayout().equals( rbdBndl.getPaneLayout() ) ) {
    			     System.out.println("PaneLayouts of the RBD and Editor don't match?");
    			continue;
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

    		    	// if the editor was just created and there was an error, close the editor.
    		    	// TODO: if there is an error, prompt if the user wishes to continue.
    		    	if( loadResourceBundleDefn( displayPane, 
    		    			(NCMapRenderableDisplay) mapDisp, 
    		    			rbdBndl.getTimeMatcher() ) == false ) {

    		    		VizApp.runAsync(new ErrorMsg("Error Loading Pane "+paneid.toString()+
    		    				" for RBD "+ rbdBndl.getRbdName() ) );
    		    	}
    		    }
    		}

    		editor.refresh();
    		editor.refreshGUIElements();
    	}

    	// update Menu Elements for the editor
    	
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
    		ResourceName domRscName = timeMatcher.getDominantResourceName();
    		
    		if( domRscName != null && domRscName.isForecastResource() ) {
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

    	pane.setRenderableDisplay( mapDisplay );
    	pane.resize();
    	pane.refresh();

    	return true;
    }    
}