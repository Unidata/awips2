package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.eclipse.jface.dialogs.MessageDialog;
import org.geotools.coverage.grid.ImageGeometry;
import org.hibernate.ejb.AvailableSettings;

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
 * 11/18/2012   #630       Greg Hull       construct renderableDisplay from PredefinedArea
 * 11/22/2012   #630       Greg Hull       store Predefined Areas from Resources or Displays
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */

public class RscBundleDisplayMngr  {
	
	public static class PaneSelectionData {
		
		private Vector<ResourceSelection> seldResources = new Vector<ResourceSelection>();
		
//		private IGridGeometryProvider areaProvider = null;
		private PredefinedArea area = null;
	
		// we could use this if we want to be able to reset the area's if
		// the user accidently changes all of them with the geoSync button.
		//private String  prevSeldGeogArea = null;

		public PaneSelectionData( ) {
			seldResources = new Vector<ResourceSelection>();
			String areaName = NmapCommon.getDefaultMap();
			
			try {
//				areaProvider = (IGridGeometryProvider) PredefinedAreasMngr.getPredefinedArea(areaName);
				area = PredefinedAreasMngr.getPredefinedArea(areaName);
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
		public PredefinedArea getArea() {
			return area;
		}

		public void setArea( PredefinedArea pa ) {
			area = pa;
		}

		public void setZoomLevel( String zl ) {
			area.setZoomLevel( zl );
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
	
	// the initial timeMatcher from an RbdBundle. This may be superceded by 
	// any changes from the GUI.
	private NCTimeMatcher initialTimeMatcher = null; 
	
	private Map<String,PredefinedArea> availPredefinedAreas = 
								new HashMap<String,PredefinedArea>();
	
	// TODO : change this to a Predefined area created from the IGridGeometryProvider (ie satellite/radar/grid)
	// so that we can save the zoomLevel (ex FitToScreen or SizeOfImage)
	//
	private Map<String,List<IGridGeometryProvider>> availAreaProvidersMap = 
		                        new HashMap<String,List<IGridGeometryProvider>>();
//	private Map<String,List<PredefinedArea>> availAreaProvidersMap = 
//        new HashMap<String,List<PredefinedArea>>();

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
		
		availPredefinedAreas.clear();
		
		for( String pAreaName : PredefinedAreasMngr.getAvailPredefinedAreas() ) {
			if( !availPredefinedAreas.containsKey( pAreaName ) ) {
				
				try {
					PredefinedArea parea = PredefinedAreasMngr.getPredefinedArea(pAreaName); 
					availPredefinedAreas.put( pAreaName, parea );
//					System.out.println("adding area "+pAreaName+" "+parea.getProviderName()+ " "+parea.getAreaName() );
				} catch (VizException e) {
					System.out.println("Error creating Area, "+pAreaName+": "+ e.getMessage() );
				}
//				availAreaProvidersMap.put( pAreaName, new ArrayList<IGridGeometryProvider>() );
			}
//			availAreaProvidersMap.get( pAreaName ).add(
//					PredefinedAreasMngr.getPredefinedArea( pAreaName );)
		}

		availAreaProvidersMap.clear();		
		
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
		
		// loop through each of the panes and initialize the paneData to the
		// selections in the rbd.
    	for( int r=0 ; r<paneLayout.getRows() ; r++ 	) {
    		for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {
    			PaneID paneId = new PaneID(r,c);
    			
    			setSelectedPaneId( paneId );
    			
    			NCMapRenderableDisplay dispPane = rbdBndl.getDisplayPane(paneId);
    			
    			setPaneData( dispPane );
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
	
	// used when importing a single pane. In this case the display-defined
	// area has not been added as an available selection.
	//
	public void setPaneData( NCMapRenderableDisplay dispPane ) throws VizException {

		// Note that the currently selected paneId doesn't have to match the 
		// dispPane's paneId.
		//

		// loop through the resources and add them 
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
		//how to set size-of-image if the imported zoom level is not -1? we can't even tell if 
//		it has been changed from the original size-of-image???

		// set the areaName and if not a regular PredefinedArea (ie from 
		// a resource or from a display that has been changed from the original 
		// area, then we need to add this as an option in the list.
    	// (we have to do this after setting the selected resources since 
    	// for resource specified areas the area must be in the list of available areas)
		//
		PredefinedArea currArea = dispPane.getCurrentArea();
		PredefinedArea initialArea = dispPane.getInitialArea();

		// the name of the selected area will be the current area of the imported display
		// unless the area has not been changed and the initial area is a predefined area
		String seldAreaName = currArea.getAreaName();

		addAvailAreaProvider( currArea );
		
		if( initialArea != null ) { // sanity check ; should only happen for out of date Rbds.
			
			if( initialArea.getAreaSource() != AreaSource.DISPLAY_AREA ) {
				
				if( PredefinedArea.areAreasEqual( initialArea, currArea ) ) {
					
					seldAreaName = initialArea.getAreaName();					
    	}
				else if( initialArea.getAreaSource() == AreaSource.RESOURCE_DEFINED ) {
    	
					// set the selected area but also need to update the area with the zoom level
					// since it was created (when the resource was added) with the default zoom level.
					//     TODO : will need to change the availAreaProvidersMap to be PredefinedAreas in order to support this.
					//
					seldAreaName = initialArea.getAreaName();					

					if( initialArea.getZoomLevel().equals( ZoomLevelStrings.SizeOfImage.toString() ) ) {						
					}
				}
			}
    	}
    	else {
			System.out.println("Initial  Area not set in RBD???");
    	}

		try {
			setAreaProviderName( seldAreaName );
		}
		catch ( VizException ve ) {
			setAreaProviderName( NmapCommon.getDefaultMap() );
		}
    	
//		setSelectedPaneId( paneId );    	 
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
	
	public void addAvailAreaProvider( ResourceSelection rscSel ) {
		if( !(rscSel.getResourceData() instanceof IGridGeometryProvider) ) {
			return;
		}

		addAvailAreaProvider( (IGridGeometryProvider)rscSel.getResourceData() );
	}
	
	public void addAvailAreaProvider( IGridGeometryProvider geomPrvdr ) {
		if( geomPrvdr instanceof AbstractNatlCntrsRequestableResourceData ) {
			
			if( !availAreaProvidersMap.containsKey( geomPrvdr.getProviderName() ) ) {
				availAreaProvidersMap.put( geomPrvdr.getProviderName(), new ArrayList<IGridGeometryProvider>() );
			}
			availAreaProvidersMap.get( geomPrvdr.getProviderName() ).add( geomPrvdr );
		}
		else if( geomPrvdr instanceof PredefinedArea ) {
			if( !availPredefinedAreas.containsKey( geomPrvdr.getProviderName() ) ) {
				availPredefinedAreas.put( geomPrvdr.getProviderName(), (PredefinedArea)geomPrvdr );
			}
		}
	}

	// if null rsc then this means to remove all of the resources
	//
	public void removeAvailAreaProvider( ResourceSelection rscSel ) {
		if( rscSel == null ||			
		  !(rscSel.getResourceData() instanceof IGridGeometryProvider) ) {
			return;
		}
		
		IGridGeometryProvider geomPrvdr = (IGridGeometryProvider)rscSel.getResourceData();
		
		if( availAreaProvidersMap.containsKey( geomPrvdr.getProviderName() ) ) {
			
			List<IGridGeometryProvider> list = availAreaProvidersMap.get( geomPrvdr.getProviderName() );
	
			if( !list.contains( geomPrvdr ) ) {
				// sanity check. this should be in the list
				System.out.println("Sanity Check: removing Area-capable Resource, "+rscSel.getResourceData().getResourceName()+
						", that is not in the list of available areas???");
			}
			list.remove( geomPrvdr );
			
			if( list.isEmpty() ) {
				availAreaProvidersMap.remove( geomPrvdr.getProviderName() );
				
				// Check if this is the currently selected areaName and if so reset the area to the default
				//
				for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
					if( paneData.getArea().getProviderName().equals( geomPrvdr.getProviderName() ) ) {
							paneData.setArea( 
									availPredefinedAreas.get( NmapCommon.getDefaultMap() ) );
					}
				}
			}
		}
		else {
			System.out.println("Sanity Check: removing Area-capable Resource, "+geomPrvdr.getProviderName()+
						", that is not in the map of available areas???");
		}
	}
	
	// return 
	public List<IGridGeometryProvider> getAvailAreaProviders() {
		List<IGridGeometryProvider> areaProviderResources = new ArrayList<IGridGeometryProvider>();
		List<ResourceName> providerRscNames = new ArrayList<ResourceName>();

		// the default first.
		areaProviderResources.add(
				(IGridGeometryProvider) availPredefinedAreas.get( NmapCommon.getDefaultMap() ) );

		// then any possible area-capable resources
		for( int r=0 ; r<paneLayout.getRows() ; r++ ) {
			for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {
				PaneSelectionData paneData = paneSelectionDataMap.get( new PaneID(r,c).toString() );

				for( ResourceSelection rscSel : paneData.getSelectedResources() ) {
					if( rscSel.getResourceData() instanceof IGridGeometryProvider ) {
						ResourceName rName = rscSel.getResourceName();

						if( !providerRscNames.contains( rscSel.getResourceName() ) ) {
							providerRscNames.add( rscSel.getResourceName() );
							areaProviderResources.add( 
									(IGridGeometryProvider)rscSel.getResourceData() );
						}
					}
				}
			}
		}
		
		// then any areas imported from the current Display
		//
		for( PredefinedArea pArea : availPredefinedAreas.values() ) {
			if( pArea.getAreaSource() == AreaSource.DISPLAY_AREA ) {
				 areaProviderResources.add( pArea );
			}
		}

		// and then the non-default predefined areas		
		for( PredefinedArea pArea : availPredefinedAreas.values() ) {
			
			if( pArea.getAreaSource() == AreaSource.PREDEFINED_AREA &&
				!pArea.getAreaName().equals( NmapCommon.getDefaultMap() ) ) {
				
				 areaProviderResources.add( pArea );
			}
		}

		return areaProviderResources;
	}

	public boolean addSelectedResource( ResourceSelection rbt ) {
		rbdModified = true;

		boolean retval = selectedPaneData.addSelectedResource( rbt );
		
		addAvailAreaProvider( rbt );
		
		return retval;
		
	}
	
	public boolean addSelectedResourceToAllPanes( ResourceSelection rbt ) {
		for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
			paneData.addSelectedResource( rbt );
		}
		
		addAvailAreaProvider( rbt );
		
		return true;
	}


	public boolean replaceSelectedResource( ResourceSelection existingRsc, ResourceSelection newRsc ) {
		rbdModified = true;
		boolean retval = selectedPaneData.replaceSelectedResource( existingRsc, newRsc );
		removeAvailAreaProvider( existingRsc );
		addAvailAreaProvider( newRsc );
		return retval;
	}

	public void removeSelectedResource( ResourceSelection rbt ) {
		rbdModified = true;
		selectedPaneData.removeSelectedResource( rbt );
		removeAvailAreaProvider( rbt );
	}
	
	public void removeAllSelectedResources( ) {
		rbdModified = true;
		selectedPaneData.removeSelectedResource( null );
		removeAvailAreaProvider( null );
	}
	
	public void removeAllSelectedResourcesForPane( PaneID paneId ) {
		if( !paneSelectionDataMap.containsKey( paneId.toString() ) ) {
			return;
		}
		rbdModified = true;
		PaneSelectionData psd = paneSelectionDataMap.get( paneId.toString() );
		for( ResourceSelection rs : psd.getSelectedResources() ) {
			removeAvailAreaProvider( rs );
			psd.removeSelectedResource( rs );
		}		
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

	public PredefinedArea getAreaProvider() {
		return selectedPaneData.getArea();//.getAreaName();//GeoAreaName();
	}
	
	// this has to be a valid/available areas. either a predefined area or a resource.
	//
	// If multi-pane and if geoSync is set then we will need to update all of the 
	public void setAreaProviderName( String areaName ) throws VizException {
		
//		IGridGeometryProvider areaProvider;
		PredefinedArea seldArea;

		// look up the area name in the map of available areas and set 
		// 
		// if this is not a valid predefinedArea then don't set
		if( availPredefinedAreas.containsKey( areaName ) ) {
			seldArea = availPredefinedAreas.get( areaName );
//			areaProvider = (IGridGeometryProvider) availPredefinedAreas.get( areaName );
		}
		else if( availAreaProvidersMap.containsKey( areaName ) ) {
			List<IGridGeometryProvider> areaList = availAreaProvidersMap.get( areaName );
			
			// if null then assume it is 
			if( areaList == null || areaList.isEmpty() ) { // sanity check the (maybe empty) list should be created already
				throw new VizException("Unable to set the Area to "+areaName+".\n "+
							"Area or Resource is not available (null list sanity check).");
			}
			else { // get the first. It doesn't matter which.
				// create a PredefinedArea from the 
				seldArea = new PredefinedArea( AreaSource.RESOURCE_DEFINED,
					areaList.get(0).getProviderName(),					
					areaList.get(0).getGridGeometry(),
					areaList.get(0).getMapCenter(),
					areaList.get(0).getZoomLevel() );
			}			
		} 
		else {
			throw new VizException("Unable to set the Area to "+areaName+".\n "+
									"Area or Resource is not available for selection.");
		}
		
		rbdModified = true;
		
		if( isMultiPane() && isGeoSyncPanes() ) {
			for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
//				paneData.setAreaProvider( areaProvider );
				paneData.setArea( seldArea );
			}
		}
		else {
//			selectedPaneData.setAreaProvider( areaProvider );
			selectedPaneData.setArea( seldArea );
		}
	}

	public void setZoomLevel( String  zl ) {
		if( isMultiPane() && isGeoSyncPanes() ) {
			for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
				paneData.setZoomLevel( zl );
			}
		}
		else {
			selectedPaneData.setZoomLevel( zl );
		}
		
	}
	
	// update all of the geoAreaNames to the area in the current pane
	// TODO : what about hidden panes?
	public void syncPanesToArea( ) {
		setGeoSyncPanes( true );
//		IGridGeometryProvider seldArea = getAreaProvider();
		PredefinedArea seldArea = getAreaProvider();
		
		for( int r=0 ; r<paneLayout.getRows() ; r++ ) {
			for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {
				paneSelectionDataMap.get( new PaneID(r,c).toString() ).setArea( seldArea );
									//setAreaProvider( seldArea );
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
   				
// create a renderable display with the paneId as the name and the selected initial predefinedArea
//   				IGridGeometryProvider geomPrvdr = paneData.getAreaProvider();
   				
   		        PredefinedArea pArea = paneData.getArea();

   				NCMapRenderableDisplay dispPane =
   					new NCMapRenderableDisplay( paneId, pArea );
   						

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