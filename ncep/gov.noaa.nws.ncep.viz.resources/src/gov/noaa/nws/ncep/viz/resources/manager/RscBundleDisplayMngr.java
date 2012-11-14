package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.eclipse.jface.dialogs.MessageDialog;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;


/**
 * Mngr class for the Selection and Creation of RBDs from the Resource 
 * Bundle Display Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/1/08		#24			Greg Hull		Initial creation
 * 12/15/08     #43     	Greg Hull    	Call NmapCommon methods
 * 06/22/09     #115        Greg Hull       Sort overlays. Integrate with new RBTemplate
 * 07/16/09     #139        Greg Hull       Template creates the rsc name now.
 * 08/10/09                 Greg Hull       get attrSet filename from NmapCommon method
 * 10/19/09     #145        Greg Hull       Add seld rscs to top. Added Make Dominant Resource
 * 01/15/10     #202        B. Hebbard      In addSeldRscToList() add prompt for PGEN XML file
 * 01/20/10     #217        Greg Hull       Use modified ResourceBndlTemplate.
 * 01/26/10     #226        Greg Hull       Removed mngmt of resource selection values.
 * 02/24/10     #226        Greg Hull       Reworked for PaneSelectionData and paneSelectionDataMap
 * 04/11/10     #259        Greg Hull       unmarshal predefined area file instead of calling utility
 *                                          to avoid cyclical dependency
 * 08/10/10     #273        Greg Hull       use ResourceName; call util to get predefinedArea
 * 08/18/10     #273        Greg Hull       getHiddenPaneIds, rbdName
 * 09/01/10     #307        Greg Hull       implement autoUpdate
 * 02/20/11     #408        Greg Hull       add initialTimeMatcher
 * 04/26/11     #416        M. Gao          Add RscBundleDisplayMngr deep copy functionality 
 * 07/11/11                Greg Hull       Back out #416 changes supporting SPF Mngr.
 * 10/22/11     #467       Greg Hull       replace selected resource
 * 01/05/11     #561       Greg Hull       add Locator as default selected resource
 * 06/21/12     #646       Greg Hull       save PredefinedArea obj so we can save the 
 *                                         mapCenter/extents/zoomLevel
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */

public class RscBundleDisplayMngr  {
	
	public static class PaneSelectionData {
		
		private Vector<ResourceSelection> seldResources = new Vector<ResourceSelection>();
		
		private PredefinedArea predefinedArea = null;
	
		// we could use this if we want to be able to reset the area's if
		// the user accidently changes all of them with the geoSync button.
		//private String  prevSeldGeogArea = null;

		public PaneSelectionData( ) {
			seldResources = new Vector<ResourceSelection>();
			String areaName = NmapCommon.getDefaultMap();
			
			try {
				predefinedArea = PredefinedAreasMngr.getPredefinedArea(areaName);
			} catch (VizException e1) {
				System.out.println("Error getting default PredefinedArea, "+areaName);
			}
			
			if( baseOverlayRBT == null ) {
				// add the 'Base' (ie geoPolitical) overlay to the list of selected Resources
				ResourceName rscName = new ResourceName( 
						ResourceName.OverlayRscCategory, NmapCommon.getBaseOverlay(), null ); // use default attrSet

				try {
					baseOverlayRBT = ResourceFactory.createResource( rscName ); 				

				} catch (VizException e) {
					MessageDialog errDlg = new MessageDialog( 
							NmapUiUtils.getCaveShell(), 
							"Error", null, 
							"Error creating base overlay :"+rscName.toString()+"\n"+e.getMessage(),
							MessageDialog.ERROR, new String[]{"OK"}, 0);
					errDlg.open();
				}
			}

			seldResources.add( baseOverlayRBT );
		}
		
		public PredefinedArea getPredefinedArea() {
			return predefinedArea;
		}

		public void setPredefinedArea( PredefinedArea pArea ) throws VizException {
			predefinedArea = PredefinedAreasMngr.clonePredefinedArea( pArea );
		}

		public void setPredefinedArea( String areaName ) {
			try {
				predefinedArea = PredefinedAreasMngr.getPredefinedArea(areaName);
			} catch (VizException e1) {
				System.out.println("Error getting default PredefinedArea, "+areaName);
			}
		}

		public ResourceSelection[] getSelectedResources() {
			return seldResources.toArray( new ResourceSelection[0] );
		}		

		public boolean addSelectedResource( ResourceSelection rbt ) {
			for( ResourceSelection r : seldResources ) {
				if( rbt.getResourceName().toString().equals( r.getResourceName().toString() ) ) {
					return false;					
				}
			}
			seldResources.add( 0, rbt );
			
			return true;
		}

		public boolean replaceSelectedResource( ResourceSelection existingRsc, ResourceSelection newRsc ) {
			// find the existing resource and replace it.
			int rscIndx = seldResources.indexOf( existingRsc );
			
			// if we can't replace, just add it.
			if( rscIndx == -1 ) {
				seldResources.add( newRsc );
				return false;
			}
			else {
				
				if( existingRsc.getResourceName().toString().equals( baseOverlayRBT.getResourceName().toString() ) ) {
					System.out.println("Can't replace the base Overlay :" + NmapCommon.getBaseOverlay() );
					seldResources.add( newRsc );
					return false;
				}
				else {
					seldResources.set( rscIndx, newRsc );
					return true;
				}				
			}
		}

		public void resetPane() {
			seldResources.clear();
			
			// if clearing all resources we will remove any edits made to the base overlay.
			if( baseOverlayRBT == null ||
				baseOverlayRBT.getResourceData().getIsEdited() ) {
				try {
					ResourceName rscName = new ResourceName( 
							ResourceName.OverlayRscCategory, NmapCommon.getBaseOverlay(), null );
					baseOverlayRBT = ResourceFactory.createResource( rscName ); 				
				} catch (VizException e) {
					System.out.println( "Error creating base overlay??" );
				}
			}

			seldResources.add( baseOverlayRBT );
			
		}
		
		// pass in null to remove all of the resources (except the base.)
		public void removeSelectedResource( ResourceSelection rbt ) {
			if( rbt == null ) {
				resetPane();
			}
			else {
				if( seldResources.contains( rbt ) ) { // sanity check; this has to be in the list
					if( rbt.getResourceName().toString().equals( baseOverlayRBT.getResourceName().toString() ) ) {
						System.out.println("Can't remove the base Overlay :" + NmapCommon.getBaseOverlay() );
					}
					else {
						seldResources.remove( rbt );
					}				
				}
			}
		}
	}

	// the currently selected layout (numRows,numColumns)
	private PaneLayout paneLayout = new PaneLayout(1,1);
	
	// the currently selected pane
	private PaneID selectedPaneId = new PaneID(0,0);
	
	// this will map the paneID (as a string) to the current area and resource 
	// information .
	private HashMap<String, PaneSelectionData> paneSelectionDataMap = 
		               new HashMap<String,PaneSelectionData>();
	
	// the entry in the paneSelectionDataMap that is currently selected
	private PaneSelectionData selectedPaneData = null;
	
	private static ResourceSelection baseOverlayRBT = null; // the geopolitical overlay
	
	private boolean geoSyncPanes = false;		
	private boolean autoUpdate = false;		

	private boolean multiPane = false;
	
	private String rbdName = "";
	
	// set to true here if any of the resources, panelayout,... is changed. Reset to false here
	// if initialized. Set to true from the dialog if a resource is edited or if the rbd is cleared.
	private boolean rbdModified = false;
	
	private List<String> availGeoAreas = null;
	
	// the initial timeMatcher from an RbdBundle. This may be superceded by 
	// any changes from the GUI.
	private NCTimeMatcher initialTimeMatcher = null; 
	
	public List<String> getAvailGeoAreas() {
		return availGeoAreas;
	}

	public boolean isRbdModified() {
		return rbdModified;
	}

	public void setRbdModified(boolean rbdModified) {
		this.rbdModified = rbdModified;
	}

	public boolean isMultiPane() {
		return multiPane;
	}

	public void setMultiPane(boolean multiPane) {
		if( this.multiPane == multiPane ) {
			return;
		}
		rbdModified = true;
		
		this.multiPane = multiPane;
	}

	public PaneLayout getPaneLayout() {
		return paneLayout;
	}
	
	// return the selected paneId so the caller may tell if it changed.
	public PaneID setPaneLayout(PaneLayout paneLayout) {
		if( this.paneLayout.equals( paneLayout ) ) {
			return selectedPaneId;
		}
		
		rbdModified = true;
		
		if( paneLayout.getRows() <= selectedPaneId.getRow() ) {
			selectedPaneId = new PaneID( selectedPaneId.getRow()-1, 
										 selectedPaneId.getColumn() );
		}
		if( paneLayout.getColumns() <= selectedPaneId.getColumn() ) {
			selectedPaneId = new PaneID( selectedPaneId.getRow(),
										 selectedPaneId.getColumn()-1 );		
		}
		
		this.paneLayout = paneLayout;
		
		setMultiPane( paneLayout.getNumberOfPanes() > 1 );
		
		// ?? If we are reducing the size of the layout then should we remove 
		// pane outside of the new layout??
		
		// make sure there is an entry in the map for each pane
		for( int r=0 ; r<paneLayout.getRows() ; r++ ) {
			for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {
				if( !paneSelectionDataMap.containsKey( new PaneID(r,c).toString() ) ) {
					paneSelectionDataMap.put( new PaneID(r,c).toString(), 
						                   	  new PaneSelectionData() );
				}
			}
		}		
		return selectedPaneId;
	}

	public String getRbdName() {
		return rbdName;
	}

	public void setRbdName(String rbdName) {
		if( this.rbdName.equals( rbdName ) ) {
			return;
		}
		rbdModified = true;
		
		this.rbdName = rbdName;
	}

	public PaneID getSelectedPaneId() {
		return selectedPaneId;
	}

	public void setSelectedPaneId( PaneID seldPaneId ) {
		// TODO : Should this set rbdModified? No.
		// rbdModified = true;
		
		if( paneLayout.getRows() <= seldPaneId.getRow() ||
			paneLayout.getColumns() <= seldPaneId.getColumn() ) {
		
			System.out.println("Attempting to select a paneID ("+seldPaneId.toString()+
					") that is not in the current layout: "+ paneLayout.toString() );
			return;
		}
		selectedPaneId = seldPaneId;
		selectedPaneData = paneSelectionDataMap.get( selectedPaneId.toString() );
		
		// This will probably be a fatal error. This should never happen.
		if( selectedPaneData == null ) {
			System.out.println("ERROR: could not find the selected pane id ("+
					selectedPaneId.toString()+") in the paneLayout Map." );					
		}
	}

	// 
	public void init() {
		
		rbdModified = false;
		rbdName = "";
		paneSelectionDataMap.clear();
				
		initialTimeMatcher = new NCTimeMatcher();
				
		paneLayout = new PaneLayout(1,1);
		selectedPaneId = new PaneID(0,0);
		selectedPaneData = new PaneSelectionData();

		paneSelectionDataMap.clear();
		paneSelectionDataMap.put( selectedPaneId.toString(),
				                  selectedPaneData );		
		
		availGeoAreas = Arrays.asList( PredefinedAreasMngr.getAvailPredefinedAreas() );
		
		multiPane = false;
		
		geoSyncPanes = true;
		autoUpdate = false;
	}	
	
	public boolean initFromRbdBundle( RbdBundle rbdBndl ) throws VizException {
		init();
					
		// TODO : don't set the rbdName if it is the default RBD as a 
		
		setRbdName( rbdBndl.getRbdName() );
		setPaneLayout( rbdBndl.getPaneLayout() );
		setAutoUpdate( rbdBndl.isAutoUpdate() );
		setGeoSyncPanes( rbdBndl.isGeoSyncedPanes() );
		
    	for( int r=0 ; r<paneLayout.getRows() ; r++ 	) {
    		for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {
    			PaneID paneId = new PaneID(r,c);
    			
    			setSelectedPaneId( paneId );
    			
    			NCMapRenderableDisplay dispPane = rbdBndl.getDisplayPane(paneId);
    			
//    			setGeoAreaName( dispPane.getPredefinedAreaName() );
    			setPredefinedArea( 
    					PredefinedAreasMngr.createPredefinedArea( dispPane ) );

    	    	for( ResourcePair rp : dispPane.getDescriptor().getResourceList() ) {
    	    		if( rp.getResourceData() instanceof INatlCntrsResourceData ) {
    	    			try {
    	    				ResourceSelection rscSel = 
    	    					ResourceFactory.createResource( rp );
    	        			
    	    				addSelectedResource( rscSel );	
    	        			
    	        			if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
//    	        				timelineControl.addAvailDomResource( (AbstractNatlCntrsRequestableResourceData) rbt.getResourceData() );
    	        			}    			
    	    			}
    	    			catch( VizException ve ) {
    	        			System.out.println("Unable to load Resource:"+
    	        				((INatlCntrsResourceData)rp.getResourceData()).
    	        				               getResourceName().toString() );
    	        			continue;
    	    			}
    	    		}
    	    		else if( !rp.getProperties().isSystemResource() ) {
    	    			System.out.println("Unable to load non-NC non-System Resource:"+rp.getResourceData().toString() );
    	    		}
    	    	}

    			setSelectedPaneId( paneId );    	   		
    		}
    	}
    	
    	setSelectedPaneId( rbdBndl.getSelectedPaneId() );

    	if( rbdBndl.getTimeMatcher() == null ) {
    		initialTimeMatcher = new NCTimeMatcher();
    	}
    	else {
    		initialTimeMatcher = rbdBndl.getTimeMatcher();    			
    	}

    	rbdModified = false;
    	
    	return true;
	}

	public ResourceSelection[] getSelectedRscs() { 	 
		return selectedPaneData.getSelectedResources();
	}

	public ResourceSelection[] getRscsForPane( PaneID paneId ) {
		if( !paneSelectionDataMap.containsKey( paneId.toString() ) ) {
			return new ResourceSelection[]{};
		}
		return paneSelectionDataMap.get( paneId.toString() ).getSelectedResources();
	}
	
	// take the Selected Rsc and create a new ResourceSelection. 
	// The ResourceSelection is added to the list of selected Rscs.
	//
//	public boolean addSelectedResource( ResourceName rscName ) throws VizException {
//		rbdModified = true;
//		return selectedPaneData.addSelectedResource( rscName );		
//	}
	
	public void setPredefinedArea( PredefinedArea pArea ) throws VizException {
		selectedPaneData.setPredefinedArea( pArea );
	}

	public boolean addSelectedResource( ResourceSelection rbt ) {
		rbdModified = true;
		return selectedPaneData.addSelectedResource( rbt );		
	}
	
	public boolean addSelectedResourceToAllPanes( ResourceSelection rbt ) {
		for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
			paneData.addSelectedResource( rbt );
		}
		
		return true;
	}


	public boolean replaceSelectedResource( ResourceSelection existingRsc, ResourceSelection newRsc ) {
		rbdModified = true;
		return selectedPaneData.replaceSelectedResource( existingRsc, newRsc );		
	}

	public void removeSelectedResource( ResourceSelection rbt ) {
		rbdModified = true;
		selectedPaneData.removeSelectedResource( rbt );
	}
	
	public void removeAllSelectedResources( ) {
		rbdModified = true;
		selectedPaneData.removeSelectedResource( null );
	}
	
	public void removeAllSelectedResourcesForPane( PaneID paneId ) {
		if( !paneSelectionDataMap.containsKey( paneId.toString() ) ) {
			return;
		}
		rbdModified = true;
		paneSelectionDataMap.get( paneId.toString() ).removeSelectedResource( null );
	}
	
	// return a list of all the Panes that are not in the layout
	// 
//	public ArrayList<PaneID> getHiddenPaneIds() {
//		ArrayList<PaneID> hiddenPanes = new ArrayList<PaneID>();
//		
//		// loop thru all of the panes in the map and if it is not 
//		// in the current layout, add it to the list
//		for( String paneIdStr : paneSelectionDataMap.keySet() ) {
//			PaneID paneId = PaneID.parsePaneId( paneIdStr );
//			
//			if( paneId.getRow() >= paneLayout.getRows() || 
//				paneId.getColumn() >= paneLayout.getColumns() ) {
//
//				hiddenPanes.add( paneId );
//			}
//		}
//
//		return hiddenPanes;
//	}

	public String getGeoAreaName() {
		return selectedPaneData.getPredefinedArea().getPredefinedAreaName();//GeoAreaName();
	}
   			
	public PredefinedArea getPredefinedArea() {
		return selectedPaneData.getPredefinedArea();
	}
	
	// If multi-pane and if geoSync is set then we will need to update all of the 
	public boolean setGeoAreaName( String areaName ) {
		// if this is not a valid predefinedArea then don't set
		if( !availGeoAreas.contains( areaName ) ) {
			return false;
		}
		
		rbdModified = true;
		
		if( isMultiPane() && isGeoSyncPanes() ) {
			for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
				paneData.setPredefinedArea( areaName ); 
			}
		}
		else {
			selectedPaneData.setPredefinedArea( areaName );
		}
		
		return true;
	}
	
	// update all of the geoAreaNames to the area in the current pane
	// TODO : what about hidden panes?
	public void syncPanesToArea( ) {
		setGeoSyncPanes( true );
		String seldArea = getGeoAreaName();
		
		for( int r=0 ; r<paneLayout.getRows() ; r++ ) {
			for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {
				paneSelectionDataMap.get( new PaneID(r,c).toString() ).
				          setPredefinedArea( seldArea );
			}
		}		
		
		rbdModified = true;		
	}
	
    public boolean isGeoSyncPanes() {
		return geoSyncPanes;
	}

	public void setGeoSyncPanes(boolean geoSyncPanes) {
		this.geoSyncPanes = geoSyncPanes;
	}
	
	public boolean isAutoUpdate() {
		return autoUpdate;
	}

	public void setAutoUpdate(boolean autoUpdate) {
		if( this.autoUpdate == autoUpdate ) {
			return;
		}

		rbdModified = true;
		
		this.autoUpdate = autoUpdate;
	}
	
	public NCTimeMatcher getInitialTimeMatcher() {
		return initialTimeMatcher;
	}

	// Create the RBD 
   	// get the resources, overlays and map background bundle files. Load each bundle and re-bundle 
   	// them into an RBD bundle file.
   	//
	public RbdBundle createRbdBundle( String rbdName, NCTimeMatcher timeMatcher ) throws VizException {
   	
   		int numRows = (isMultiPane() ? paneLayout.getRows() : 1); 
   		int numCols = (isMultiPane() ? paneLayout.getColumns() : 1); 

   		RbdBundle rbdBndl = new RbdBundle( new PaneLayout( numRows, numCols) );
   		
   		rbdBndl.setRbdName( rbdName );

   		if( timeMatcher != null ) {    	   		
   	   		rbdBndl.setTimeMatcher( timeMatcher );
   		}
   		
   		rbdBndl.setSelectedPaneId( isMultiPane() ? selectedPaneId : new PaneID(0,0) );
   		
   		rbdBndl.setAutoUpdate( autoUpdate );
   		rbdBndl.setGeoSyncedPanes( geoSyncPanes );
   		
   		for( int r=0 ; r<numRows ; r++ ) {
   			for( int c=0 ; c<numCols ; c++ ) {
   				PaneID paneId = new PaneID(r,c); 
   				
   				PaneSelectionData paneData = paneSelectionDataMap.get( paneId.toString() );
   				
   				PredefinedArea predefinedArea = paneData.getPredefinedArea();

   				NCMapRenderableDisplay dispPane = predefinedArea.getPredefinedArea();
   						
   				//dispPane.setPredefinedAreaName( paneData.getGeogAreaName() );

   				NCMapDescriptor paneDescr = (NCMapDescriptor) dispPane.getDescriptor();

   				// auto update is set for the RbdBundle and is also set for each descriptor
   				paneDescr.setAutoUpdate( autoUpdate );
   				
   	   			if( paneDescr.getResourceList() == null ) {
   	   	   			VizException ve = new VizException( "getResourceList is null??." );
   	   	   			throw ve;
   	   			}
   	   			else {
   	   				paneDescr.getResourceList().clear();
   	   			}

   	   	   		// loop thru the bundles for the selected rscs
   	 			//
   	   			for( ResourceSelection rbt : paneData.getSelectedResources() ) {
   	   				ResourcePair rscPair = rbt.getResourcePair();
//   	   				if( dfltDomRscName == null &&
//   	   					rscPair.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
//   	   					dfltDomRscName = ((INatlCntrsResourceData)
//   	   							             rscPair.getResourceData()).getFullResourceName();
//   	   				}
//   	   				if( rbt.isDominant() ) {
//   	   					selDomRsc = (AbstractNatlCntrsRequestableResourceData) rscPair.getResourceData();
//   	   				}
   	   				paneDescr.getResourceList().add( rscPair );                   
   	   			}

   	   			// set the timeMatcher for the Descriptor (This will be the same timeMatcher
   	   			// for all panes.
   	   			paneDescr.setTimeMatcher( timeMatcher );
   				
   				rbdBndl.addDisplayPane( dispPane, paneId );   		   				
   			}
   		}
   		
   		return rbdBndl;
   	}
   	    
    public ResourceSelection getBaseOverlay( ) {
    	return baseOverlayRBT;
    }
}