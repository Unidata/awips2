package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.area.AreaMenus;
import gov.noaa.nws.ncep.viz.common.area.AreaMenusMngr;
import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.AreaMenus.AreaMenuItem;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.INcAreaProviderFactory;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsPaneManager;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.IPaneLayoutable;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.eclipse.jface.dialogs.MessageDialog;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.ui.editor.AbstractEditor;


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
 * 02/20/2013   #972       Greg Hull       support new NcDisplayType
 * 05/20/2013   #862       Greg Hull       getAvailAreaMenuItems() ; rm maps to store avail areas, 
 *                                         and use new areaProvider sources.
 * 11/25/2013   #1078      Greg Hull       check for FitToScreen and SizeOfImage in setPaneData()
 * 11/25/2013   #1079      Greg Hull       checkAndUpdateAreaFromResource
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class RscBundleDisplayMngr    {
	
	public static class PaneSelectionData {
		private RscBundleDisplayMngr rscBndlMngr=null;
		
//	    private class RenderingOrderComparator implements Comparator<ResourceSelection> {
//
//	        @Override
//	        public int compare(ResourceSelection o1, ResourceSelection o2) {
//	            Integer i1 = o1.getRenderingOrder();
//	            Integer i2 = o2.getRenderingOrder();
//	            return i1.compareTo(i2);
//	        }
//	    }

		private Vector<ResourceSelection> seldResources = new Vector<ResourceSelection>();
		
		// if set with a 'custom' area defined by an RBD then this will
		// be set to the custom area. Since it can't be accessed by an areaFactory
		// until it is loaded, this is use to reset the area should the user reselect 
		// the custom area from the gui.
		private PredefinedArea initialArea = null;
		
		private PredefinedArea area = null;
		
		// we could use this if we want to be able to reset the area's if
		// the user accidently changes all of them with the geoSync button.
		//private String  prevSeldGeogArea = null;

		public PaneSelectionData( RscBundleDisplayMngr rbdm, NcDisplayType rbdType ) {
			rscBndlMngr = rbdm;
			seldResources = new Vector<ResourceSelection>();
			
			try {
				area = PredefinedAreaFactory.getDefaultPredefinedAreaForDisplayType( rbdType );
			} catch (VizException e1) {
				System.out.println("Error getting default PredefinedArea: "+e1.getMessage() );
			}
           
			// Reference the same base overlay for multi-pane displays.
			if( baseOverlayResources.containsKey( rbdType ) ) {
				seldResources.add( baseOverlayResources.get( rbdType ) );			
			}
		}
		
		public PredefinedArea getInitialArea() {
			return initialArea;
		}

		public PredefinedArea getArea() {
			return area;
		}

		// bit of a hack here. null indicates to reset to a custom area from an rbd
		// that was initially set from an earlier call to setArea() where the source
		// was the INITIAL_DISPLAY_AREA from an RBD.
		public void setArea( PredefinedArea pa ) {
			if( pa == null ) {
				area = initialArea;
			}
			else {
				area = pa;
				if( initialArea == null ||
					area.getSource() == AreaSource.INITIAL_DISPLAY_AREA ) {
					initialArea = area;
				}
			}
		}
		
		public void setZoomLevel( String zl ) {
			area.setZoomLevel( zl );
		}
		
		public ResourceSelection[] getSelectedResources() {
			return seldResources.toArray( new ResourceSelection[0] );
		}		
		
		// Don't add the same resource twice. If this is the base overlay, 
		// replace the existing base overlay.
		public boolean addSelectedResource( ResourceSelection rbt ) {
//			for( ResourceSelection r : seldResources ) {
			for( int r=0 ; r<seldResources.size() ; r++ ) {
				ResourceSelection rscSel = seldResources.elementAt(r);
				if( rbt.getResourceName().equals( rscSel.getResourceName() ) ) {
					if( rscSel.isBaseLevelResource() ) {
//						baseOverlayResources
						rbt.setIsBaseLevelResource( true );
						seldResources.add(r, rbt);
						seldResources.remove(r+1);
						return true;
					}
					return false;					
				}
			}

			seldResources.add( 0, rbt );
			
			return true;
		}

		// 
		public boolean replaceSelectedResource( ResourceSelection existingRsc, ResourceSelection newRsc ) {
			// find the existing resource and replace it.
			int rscIndx = seldResources.indexOf( existingRsc );
			
			// if we can't replace, just add it.
			if( rscIndx == -1 ) {
				seldResources.add( newRsc );
				return false;
			}
			else {				
				if( existingRsc.isBaseLevelResource() ) {
					System.out.println("Can't replace the base Overlay :" + existingRsc.getResourceName() );
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
			if( baseOverlayResources.containsKey( rscBndlMngr.getRbdType() ) ) {
				ResourceSelection baseRsc = baseOverlayResources.get( rscBndlMngr.getRbdType() );
				
				if( baseRsc != null ) { 
					if( baseRsc.getResourceData().getIsEdited() ) {						
						try {
							baseRsc = ResourceFactory.createResource( baseRsc.getResourceName() );
							
						} catch (VizException e) {
							System.out.println( "Error creating base overlay for "+
									rscBndlMngr.getRbdType().getName()+" RBDs: "+ e.getMessage() );
							return;
						}					
					}
					
					seldResources.add( baseRsc );			
				}
			}
		}
		
		// pass in null to remove all of the resources (except the base.)
		public void removeSelectedResource( ResourceSelection selRsc ) {
			if( selRsc == null ) {
				resetPane();
			}
			else {
				if( seldResources.contains( selRsc ) ) { // sanity check; this has to be in the list
					if( selRsc.isBaseLevelResource() ) {
						System.out.println("Can't remove the base Overlay :" + NmapCommon.getBaseOverlay() );
					}
					else {
						seldResources.remove( selRsc );
					}				
				}
			}
		}
	}
	
	public RscBundleDisplayMngr( NcPaneLayout maxPaneLayout, NcDisplayType dispType ) {
		
		for( NcDisplayType dt : NcDisplayType.getRbdSavableDisplayTypes() ) {
			if( dt.getBaseResource() != null && 
			   !dt.getBaseResource().isEmpty() ) {
				
				ResourceName rscName = new ResourceName( dt.getBaseResource() );
				try {					
					if( rscName.isValid() ) {								
						ResourceSelection baseOverlay  = ResourceFactory.createResource( rscName );
						baseOverlay.setIsBaseLevelResource( true );
						baseOverlayResources.put( dt, baseOverlay );
					}
					else {
						throw new VizException( "Invalid base overlay name "+rscName.toString()+" for display type: "+dt.toString() );
					}
					
				} catch (VizException e) {
					MessageDialog errDlg = new MessageDialog( 
							NcDisplayMngr.getCaveShell(), 
							"Error", null, 
							"Error creating base overlay for "+dt.getName()+" RBDs:"+rscName.toString()+"\n"+e.getMessage(),
							MessageDialog.ERROR, new String[]{"OK"}, 0);
					errDlg.open();
				}
			}
		}

		maxLayout = maxPaneLayout;
		init( dispType );
	}

	// the maximum number of rows & columns
	private NcPaneLayout maxLayout = new NcPaneLayout(6,6);
	
	// an RBD must be one of these display types and all resources in the RBD must be compatible 
	// with the display type.
	private NcDisplayType rbdType;

	// the currently selected layout (numRows,numColumns)
	private INcPaneLayout currRbdPaneLayout = new NcPaneLayout(1,1);
	
	// the currently selected pane
	private NcPaneID selectedPaneId = new NcPaneID(); // 0,0
	
	// this will map the paneID (as a string) to the current area and resource 
	// information . Note : use a String since the paneIndex is relative to the 
	// selected paneLayout which may change.
	private HashMap<String, PaneSelectionData> paneSelectionDataMap = 
		               new HashMap<String,PaneSelectionData>();
	
	// the entry in the paneSelectionDataMap that is currently selected
	private PaneSelectionData selectedPaneData = null;
	
	// base resources can't be removed and are the same instance for all panes.
	// ie. the geopolitical overlay.
	private static Map<NcDisplayType,ResourceSelection> baseOverlayResources = new HashMap<NcDisplayType,ResourceSelection>(); 
	
	private boolean geoSyncPanes = false;		
	private boolean autoUpdate = false;		

	private boolean multiPane = false;
	
	private String rbdName = "";
	
	// set to true here if any of the resources, currRbdPaneLayout,... is changed. Reset to false here
	// if initialized. Set to true from the dialog if a resource is edited or if the rbd is cleared.
	private boolean rbdModified = false;
		
	// the initial timeMatcher from an AbstractRBD<?>. This may be superceded by 
	// any changes from the GUI.
	private NCTimeMatcher initialTimeMatcher = null; 
		
	public NcPaneLayout getMaxPaneLayout() { 
		return maxLayout;
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

	// note that the currRbdPaneLayout will stay the same to hold any selections
	// in case the user turns multipane back on again. 
	public void setMultiPane(boolean multiPane) {
		if( this.multiPane == multiPane ) {
			return;
		}
		rbdModified = true;
		
		this.multiPane = multiPane;
	}

	// note that if multipane is false then only the 1,1 pane will be used
	public INcPaneLayout getPaneLayout() {
		return currRbdPaneLayout;
	}
	
	// return the selected paneId so the caller may tell if it changed.
	public INcPaneID setPaneLayout( INcPaneLayout paneLayout) {
		if( this.currRbdPaneLayout.equals( paneLayout ) ) {
			return selectedPaneId;
		}
		
		rbdModified = true;
		
//		if( paneLayout.getRows() <= selectedPaneId.getRow() ) {
//			selectedPaneId = new NcPaneID( selectedPaneId.getRow()-1, 
//										 selectedPaneId.getColumn() );
//		}
//		if( paneLayout.getColumns() <= selectedPaneId.getColumn() ) {
//			selectedPaneId = new NcPaneID( selectedPaneId.getRow(),
//										 selectedPaneId.getColumn()-1 );		
//		}
		if( !paneLayout.containsPaneId( selectedPaneId ) ) {
			selectedPaneId = new NcPaneID();
		}
			
		currRbdPaneLayout = paneLayout;
		
		setMultiPane( paneLayout.getNumberOfPanes() > 1 );
				
//		// make sure there is an entry in the map for each pane
//		for( int p=0 ; p<paneLayout.getNumberOfPanes() ; p++ ) {
//			NcPaneID pid = (NcPaneID) paneLayout.createPaneId(p);
//			if( !paneSelectionDataMap.containsKey( pid.toString() ) ) {
//				paneSelectionDataMap.put( 
//						pid.toString(), new PaneSelectionData() );
//			}
//		}
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

	public INcPaneID getSelectedPaneId() {
		return selectedPaneId;
	}

	public void setSelectedPaneId( INcPaneID seldPaneId ) {
		// TODO : Should this set rbdModified? No.
		// rbdModified = true;		
		if( !currRbdPaneLayout.containsPaneId(seldPaneId) ) {
			System.out.println("Attempting to select a paneID ("+seldPaneId.toString()+
					") that is not in the current layout: "+ currRbdPaneLayout.toString() );
			return;
		}
		selectedPaneId = (NcPaneID)seldPaneId;
		selectedPaneData = paneSelectionDataMap.get( selectedPaneId.toString() );
		
		// This will probably be a fatal error. This should never happen.
		if( selectedPaneData == null ) {
			System.out.println("ERROR: could not find the selected pane id ("+
					selectedPaneId.toString()+") in the paneLayout Map." );					
		}
	}

	// 
	public void init( NcDisplayType dispType ) {
		
		rbdType = dispType;
		
		rbdModified = false;
		rbdName = "";
		paneSelectionDataMap.clear();
		
		initialTimeMatcher = new NCTimeMatcher();
				
		// make sure there is an entry in the map for each pane and initialize it.
		for( int p=0 ; p<maxLayout.getNumberOfPanes() ; p++ ) {
			NcPaneID pid = (NcPaneID)maxLayout.createPaneId(p);			
			paneSelectionDataMap.put( 
						pid.toString(), new PaneSelectionData( this, rbdType ) );			
		}

		currRbdPaneLayout = new NcPaneLayout(1,1);
		selectedPaneId = new NcPaneID(); // 0,0
		selectedPaneData = paneSelectionDataMap.get( selectedPaneId.toString() );
				
		multiPane = false;
		
		geoSyncPanes = true;
		autoUpdate = false;
		rbdModified = false;
	}	
	
	public boolean initFromRbdBundle( AbstractRBD<?> rbdBndl ) throws VizException {
		
		// reset all panes and then set the PaneData for those in the rbd.
		init( rbdBndl.getDisplayType() );
					
		// TODO : don't set the rbdName if it is the default RBD as a 
		setRbdType( rbdBndl.getDisplayType() );
		setRbdName( rbdBndl.getRbdName() );
		setPaneLayout( rbdBndl.getPaneLayout() );
		setAutoUpdate( rbdBndl.isAutoUpdate() );
		setGeoSyncPanes( rbdBndl.isGeoSyncedPanes() );
		
		// loop through each of the panes and initialize the paneData to the
		// selections in the rbd.
    	for( int paneIndx=0 ; paneIndx<currRbdPaneLayout.getNumberOfPanes() ; paneIndx++ 	) {
    		INcPaneID paneId = currRbdPaneLayout.createPaneId(paneIndx);
    			
    		setSelectedPaneId( paneId );
    			
    		INatlCntrsRenderableDisplay dispPane = rbdBndl.getDisplayPane(paneId);
    			
    		setPaneData( dispPane );    	
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
	public void setPaneData( INatlCntrsRenderableDisplay dispPane ) throws VizException {

		// NOTE:  the currently selected paneId doesn't have to match the 
		// dispPane's paneId.
		//
		
		// loop through the resources and add them 
		for( ResourcePair rp : dispPane.getDescriptor().getResourceList() ) {
 			if( rp.getResourceData() instanceof INatlCntrsResourceData ) {
    			try {
    				ResourceSelection rscSel = ResourceFactory.createResource( rp );
        			
    				addSelectedResource( rscSel );	
        			
        			if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
//        				timelineControl.addAvailDomResource( (AbstractNatlCntrsRequestableResourceData) rbt.getResourceData() );
        			}    			
    			}
    			catch( VizException ve ) {
        			System.out.println("Unable to add selected Resource:"+
        				((INatlCntrsResourceData)rp.getResourceData()).getResourceName().toString() );
        			continue;
    			}
    		}
    		else if( !rp.getProperties().isSystemResource() ) {
    			System.out.println("Unable to load non-NC non-System Resource:"+rp.getResourceData().toString() );
    		}
    	}
		
		// how to set size-of-image if the imported zoom level is not -1? we can't even tell if 
		// it has been changed from the original size-of-image???
		//

		// set the selected areaname from the display using the following rules.
		// if this is from a current editor display and the current area has been changed from the initial area 
		// that the display was loaded with  then the selected area will be the current area of the display 
		// (the source is DISPLAY_AREA and the areaname is the name of the display pane)
		//     if from a current editor and the current and initial areas are the same then use the
		// name of the area initially used to load the display. 
		//     if this display pane is imported from and RBD then the initial area and the current area 
		// should be the same. 
//		AreaName seldAreaName = new AreaName( AreaSource.PREDEFINED_AREA, rbdType.getDefaultMap() );

		PredefinedArea initialArea = dispPane.getInitialArea();
		
//		AreaName seldAreaName = new AreaName( initialArea.getSource(), initialArea.getAreaName() );
		PredefinedArea seldPredefinedArea = initialArea;
		
		// if the display's current area is different than its initial area then  
		// set the display as the selected area otherwise use the name of the area
		// originally used to set the display's area.
		NcDisplayName dispName = dispPane.getPaneName().getDispName();				

		if( dispName.getId() > 0 &&  //dispPane.getPaneManager() instanceof NCPaneManager &&
			dispPane instanceof IAreaProviderCapable ) {
			
			// the name of the display's area will be the name of the pane.
			AreaName currDispAreaName = new AreaName( AreaSource.DISPLAY_AREA,	
														((IAreaProviderCapable)dispPane).getAreaName() );			
			try {
				IGridGeometryProvider currGeom = NcAreaProviderMngr.createGeomProvider( currDispAreaName );
				
				// the name of the selected area will be the current area of the imported display
				// unless the area has not been changed and the initial area is a predefined area
				if( currGeom != null && initialArea != null ) { // sanity check 
					
					PredefinedArea currArea = PredefinedAreaFactory.createPredefinedArea(currGeom);
					
					// The areAreasEqual method doesn't work on SizeOfImage. This will lock the zoom
					// so we will assume that if the initial area is SizeOfImage that the initial area
					// is the 
					if( !initialArea.getZoomLevel().equals( ZoomLevelStrings.SizeOfImage.toString() ) &&
						!initialArea.getZoomLevel().equals( ZoomLevelStrings.FitToScreen.toString() ) ) {
					
						// use the area of the current display if the areas are different
						// or if the same but the initial area is not a named/saved area name.
						// 
						if( PredefinedArea.areAreasEqual( initialArea, currArea ) ) {
							if( initialArea.getSource() == AreaSource.INITIAL_DISPLAY_AREA ) {
								//							seldAreaName = currDispAreaName;
								seldPredefinedArea = currArea;
							}
						}
						else {
							//						seldAreaName = currDispAreaName;
							seldPredefinedArea = currArea;
						}
					}
				}
			}
			catch ( VizException ve ) {
				System.out.println("error getting curr area for "+dispPane.getPaneName() );
			}
		}

		try {
			setSelectedArea( seldPredefinedArea );
		}
		catch ( VizException ve ) {
			setSelectedAreaName( 
					 new AreaName( AreaSource.PREDEFINED_AREA, rbdType.getDefaultMap() ) );
		}
    	
//		setSelectedPaneId( paneId );    	 
	}

	public ResourceSelection[] getSelectedRscs() { 	 
		return selectedPaneData.getSelectedResources();
	}

	public ResourceSelection[] getRscsForPane( INcPaneID paneId ) {
		if( !paneSelectionDataMap.containsKey( paneId.toString() ) ) {
			return new ResourceSelection[]{};
		}
		return paneSelectionDataMap.get( paneId.toString() ).getSelectedResources();
	}
		
	// if any of the selected areas (any pane) is resource-defined then check to make sure 
	// it is still available and if not reset to the given rsc or the default if null.
	//   return true if an area was changed.
	// 
	public Boolean checkAndUpdateAreaFromResource( ResourceSelection rscSel ) {

		// get a map of all of the currently selected areas 
		// 
		Map<String,AreaName> seldAreaNames = new HashMap<String,AreaName>();
		
		// if geoSynced then there will be only one selected area and if
		// it is reset then it will be reset for all panes.
		if( isGeoSyncPanes() ) {
						
			PredefinedArea seldArea = getSelectedArea();
			seldAreaNames.put( getSelectedPaneId().toString(), 
					new AreaName( seldArea.getSource(), seldArea.getAreaName() ) );
		}
		else {
			seldAreaNames = getAllSelectedAreaNames();
		}
		
		// determine what to reset an area to
		Boolean areaChanged = false;
		AreaName resetAreaName;

		// all available resource defined areas (from all panes)
		List<AreaMenuItem> availRscAreas = getResourceProvidedAreas();

		if( rscSel != null &&
			rscSel.getResourceData() instanceof IAreaProviderCapable ) {

			// NOTE : assume that the rscSel is valid. Dont need to check that
			// it is in the availRscAreas list.
			IAreaProviderCapable aProv = (IAreaProviderCapable)rscSel.getResourceData();
			resetAreaName = new AreaName( aProv.getSourceProvider(), aProv.getAreaName() );
			
			// if replaceing a rsc-defined area with another in a single pane or 
			// in a geo-synced multipane then go ahead and replace the area
			// 
			if( !isMultiPane() || isGeoSyncPanes() ) {			
				if( seldAreaNames.get( getSelectedPaneId().toString() ).getSource().isImagedBased() ) { 	
					try {
						setSelectedAreaName( resetAreaName );
						return true;
					} catch (VizException e) {
						System.out.println("Error reseting area??? : "+e.getMessage() );
						return false;
					}				
				}
			}			
		}
		else {
			resetAreaName = new AreaName( AreaSource.PREDEFINED_AREA, rbdType.getDefaultMap() );		
		}

		// loop thru the panes and check for resource-defined areas
		for( String paneIdStr : seldAreaNames.keySet() ) {
			AreaName areaName = seldAreaNames.get( paneIdStr );

			if( !areaName.getSource().isImagedBased() ) {
				continue;
			}
		
			Boolean areaRscIsAvailable = false;
			
			for( AreaMenuItem ami : availRscAreas ) {
				// if we have found the resource for the given area
				// then continue to the next pane
				if( AreaSource.getAreaSource( ami.getSource()) == areaName.getSource() &&
						ami.getAreaName().equals( areaName.getName() ) ) {						
					areaRscIsAvailable = true;					
					break;
				}
			}
			
			if( areaRscIsAvailable ) {
				continue;
			}
			
			// reset this pane's area to either the default or to the given rsc (ie a replace. 
			// note : if geosync is set then this will reset all of the areas
			//
			try {
				INcPaneID curPid = getSelectedPaneId();
				setSelectedPaneId( NcPaneID.parsePaneId( paneIdStr ) );
				
				setSelectedAreaName( resetAreaName );
				
				setSelectedPaneId( curPid );
				
				areaChanged = true;

			} 
			catch (VizException e) {
				System.out.println("Error resetting area to "+resetAreaName.toString()+
						": "+ e.getMessage() );				
			}
		}
		
		return areaChanged;
	}
	
	// return 
	public List<List<AreaMenuItem>> getAvailAreaMenuItems() {
		List<List<AreaMenuItem>> areaMenuItems = new ArrayList<List<AreaMenuItem>>(); 
		AreaMenuItem ami, seldami, dfltami;
	    List<AreaMenuItem> amiList;
	    AreaName areaName;
	    
		// ?? create area factories for NTRANS and SOLAR (just one 'default' area for now...)
		if( rbdType == NcDisplayType.NTRANS_DISPLAY ||
			rbdType == NcDisplayType.SOLAR_DISPLAY ) {
			areaMenuItems.add( new ArrayList<AreaMenuItem>() );
			areaMenuItems.get(0).add( 
					new AreaMenuItem( new AreaName( AreaSource.PREDEFINED_AREA, rbdType.getDefaultMap() ) ) ); 
			return areaMenuItems;
		}

	    // first is the selected area (not in a submenu)
	    amiList = new ArrayList<AreaMenuItem>();
	    areaName = new AreaName( getSelectedArea().getSource(), getSelectedArea().getAreaName() );
	    
	    if( areaName.getSource() == AreaSource.INITIAL_DISPLAY_AREA ) {
		    seldami = new AreaMenuItem( "Custom", "", areaName.getName(), areaName.getSource().toString() );
	    }
	    else {
		    seldami = new AreaMenuItem( areaName );	    	
	    }

	    amiList.add( seldami );
	    areaMenuItems.add( amiList );

	    amiList = new ArrayList<AreaMenuItem>();

	    // if the initial area is custom and not seleced then add it since it won't be saved 
	    // 
	    PredefinedArea initArea = selectedPaneData.getInitialArea(); 
	    if( initArea != null && initArea != getSelectedArea() &&
	    	initArea.getSource() == AreaSource.INITIAL_DISPLAY_AREA ) {

	    	seldami = new AreaMenuItem( "Custom", "",
	    			initArea.getAreaName(), initArea.getSource().toString() );	    							  
		    amiList.add( seldami );
		    areaMenuItems.add( amiList );
	    }
	    
		// the default next if its not selected.
	    if( getSelectedArea().getAreaName().equals( rbdType.getDefaultMap() ) ) {
	    	dfltami = seldami;
	    }
	    else {
	    	amiList = new ArrayList<AreaMenuItem>();
	    	areaName = new AreaName( AreaSource.PREDEFINED_AREA, rbdType.getDefaultMap() );
	    	dfltami = new AreaMenuItem( areaName );
	    	amiList.add( dfltami );
	    	areaMenuItems.add( amiList );
	    }	    
	    
	    // next areas from the current display (if multipane then put in a submenu)
	    amiList = new ArrayList<AreaMenuItem>();
		AbstractEditor ed = NcDisplayMngr.getActiveNatlCntrsEditor();
				
		if( ed != null && NcEditorUtil.getNcDisplayType( ed ) == rbdType ) {
    		IDisplayPane[] panes = ( NcEditorUtil.arePanesGeoSynced( ed ) ? 
    				NcEditorUtil.getSelectedPanes(ed) : ed.getDisplayPanes() );

			for( IDisplayPane pane : panes ) {
				if( pane.getRenderableDisplay() instanceof IAreaProviderCapable ) {
					IAreaProviderCapable aPrv = (IAreaProviderCapable)pane.getRenderableDisplay();
					
					ami = new AreaMenuItem( aPrv.getAreaName(), 
							(panes.length > 1 ? "Display" : ""), aPrv.getAreaName(),  aPrv.getSourceProvider().toString() );
					
					amiList.add( ami );
				}
			}
		}
		
		if( !amiList.isEmpty() ) {
			areaMenuItems.add( amiList );
		}

	    // next are any area provided by a selected resource.
		// TODO : put these in submenus based on the resource type
		//for( String rscType : new String[]{ "MCIDAS", "GINI" } ) {
		// then any possible area-capable resources
	    amiList = getResourceProvidedAreas(); //new ArrayList<AreaMenuItem>();
		
		if( !amiList.isEmpty() ) {
			areaMenuItems.add( amiList );
		}

		// finally add the areas from the areaMenus file.
	    Map<String,List<AreaMenuItem>> areaMenusMap = AreaMenusMngr.getInstance().getPredefinedAreasForMenus();

	    for( List<AreaMenuItem> alst : areaMenusMap.values() ) {
	    	if( alst == null || alst.isEmpty() ||
	    		alst.get(0).getSubMenuName().isEmpty() ) {
	    		continue;
	    	}
	    	areaMenuItems.add( alst );
	    }
	    
	    amiList = new ArrayList<AreaMenuItem>();	   
	    
	    if( areaMenusMap.containsKey("") ) {
	    	for( AreaMenuItem i : areaMenusMap.get( "" ) ) {
	    		if( !seldami.equals( i ) && 
	    			!dfltami.equals( i ) ) {
	    			amiList.add( i );
	    		}
	    	}
	    }
	    
		if( !amiList.isEmpty() ) {
			areaMenuItems.add( amiList ); 
		}

		return areaMenuItems;
	}	
	
	// get a list of all available areas resource-defined areas
	// Called by getAvailAreaMenuItems and used to determine if 
	// an area is still avail afer a resource has been removed or replaced. 
	// 
	public List<AreaMenuItem> getResourceProvidedAreas() {
		List<AreaMenuItem> amiList = new ArrayList<AreaMenuItem>();
	    
		for( int paneIndx=0 ; paneIndx<currRbdPaneLayout.getNumberOfPanes() ; paneIndx++ ) {
			PaneSelectionData paneData = paneSelectionDataMap.get( 
					currRbdPaneLayout.createPaneId( paneIndx ).toString() );

			for( ResourceSelection rscSel : paneData.getSelectedResources() ) {
				if( rscSel.getResourceData() instanceof IAreaProviderCapable ) {
					
					IAreaProviderCapable areaRsc = (IAreaProviderCapable)rscSel.getResourceData();
					
					AreaName areaName = new AreaName( areaRsc.getSourceProvider(), 
											 areaRsc.getAreaName() );
					//ami = new AreaMenuItem( areaName );
					AreaMenuItem ami = new AreaMenuItem( areaRsc.getAreaName(), 
							"Resource", areaRsc.getAreaName(),  areaRsc.getSourceProvider().toString() );

					// use the source name as the sub menu name 					
//					if( rscItems == null ) {
//						rscItems = new ArrayList<AreaMenuItem>();
//						areaMenuItems.put( areaRsc.getSourceProvider().toString(),  rscItems );
//					}
	
					if( !amiList.contains( ami ) ) {
						amiList.add( ami );
					}
				}
			}
		}
		return amiList;
	}
	
	public boolean addSelectedResource( ResourceSelection rsel ) {
		if( !Arrays.asList( rsel.getSupportedDisplayTypes() ).contains( rbdType )  ) {
			System.out.println("??Can't add resource "+rsel.getRscLabel()+" because it is not"+
					" supported for display type "+rbdType.getName() );
			return false;
		}

		rbdModified = true;

		boolean retval = selectedPaneData.addSelectedResource( rsel );
				
		return retval;
		
	}
	
	public boolean addSelectedResourceToAllPanes( ResourceSelection rbt ) {
		for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
			paneData.addSelectedResource( rbt );
		}
		rbdModified = true;
		
		return true;
	}

	public boolean replaceSelectedResource( ResourceSelection existingRsc, ResourceSelection newRsc ) {
		if( !Arrays.asList( newRsc.getSupportedDisplayTypes() ).contains( rbdType )  ) {
			System.out.println("??Can't add resource "+newRsc.getRscLabel()+" because it is not"+
					" supported for display type "+rbdType.getName() );
			return false;
		}
		 
		rbdModified = true;
		boolean retval = selectedPaneData.replaceSelectedResource( existingRsc, newRsc );
				
		checkAndUpdateAreaFromResource( newRsc );
		
		return retval;
	}

	public void removeSelectedResource( ResourceSelection rbt ) {
		rbdModified = true;
		selectedPaneData.removeSelectedResource( rbt );
		checkAndUpdateAreaFromResource( null );
	}
	
	public void removeAllSelectedResources( ) {
		rbdModified = true;
		selectedPaneData.removeSelectedResource( null );
		
		checkAndUpdateAreaFromResource( null );
	}
	
//	public void removeAllSelectedResourcesForPane( INcPaneID paneId ) {
//		if( !paneSelectionDataMap.containsKey( paneId.toString() ) ) {
//			return;
//		}
//		rbdModified = true;
//		PaneSelectionData psd = paneSelectionDataMap.get( paneId.toString() );
//		for( ResourceSelection rs : psd.getSelectedResources() ) {
//			removeAvailAreaProvider( rs );
//			psd.removeSelectedResource( rs );
//		}
//	}
	
	    
    public NcDisplayType getRbdType() {
		return rbdType;
	}

	public void setRbdType(NcDisplayType rbdType) {
		this.rbdType = rbdType;
	}

//	public ResourceSelection getBaseOverlay( ) {
//    	return baseOverlayRBT;
//    }
   			
	public PredefinedArea getSelectedArea() {
		return selectedPaneData.getArea();//.getAreaName();//GeoAreaName();
	}
	
	// return a map of all the currently selected areanames (with pane id as the key) 
	public Map<String,AreaName> getAllSelectedAreaNames() {
		Map<String,AreaName> areasMap = new HashMap<String,AreaName>();
   		for( int paneIndx=0 ; paneIndx<currRbdPaneLayout.getNumberOfPanes() ; paneIndx++ ) {
			NcPaneID paneId = (NcPaneID)currRbdPaneLayout.createPaneId(paneIndx); 
			
			PaneSelectionData psd = paneSelectionDataMap.get( paneId.toString() );
			areasMap.put( paneId.toString(), new AreaName( psd.getArea().getSource(), psd.getArea().getAreaName() ) );
		}
		return areasMap;
	}

	// called when initializing the pane data
	// If multi-pane and if geoSync is set then we will need to update all of the panes
	public void setSelectedArea( PredefinedArea seldArea ) throws VizException {
		rbdModified = true;

		if( isMultiPane() && isGeoSyncPanes() ) {
			for( PaneSelectionData paneData : paneSelectionDataMap.values() ) {
				paneData.setArea( seldArea );
			}
		}
		else {
			selectedPaneData.setArea( seldArea );
		}
		
	}
	
	// called from the GUI  
	public void setSelectedAreaName( AreaName areaName ) throws VizException {
		
		//  if Custom then use null as a flag to reset to the saved initial area
		if( areaName.getSource() == AreaSource.INITIAL_DISPLAY_AREA ) {
			setSelectedArea( null );
		}
		else {
			IGridGeometryProvider geom =  NcAreaProviderMngr.createGeomProvider(areaName);
			PredefinedArea seldArea = PredefinedAreaFactory.createPredefinedArea( geom );
			if( seldArea == null ) {
				throw new VizException("Unable to set the Area to "+areaName.toString() );
			}
	
			setSelectedArea( seldArea );
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
	// Include hidden panes too in case the user expands the number of panes,
	// the area will be set correctly
	//
	public void syncPanesToArea( ) {
		setGeoSyncPanes( true );
		PredefinedArea seldArea = getSelectedArea();
		
		for( PaneSelectionData psd : paneSelectionDataMap.values() ) {
			psd.setArea( seldArea );
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
	public AbstractRBD<?> createRbdBundle( String rbdName, NCTimeMatcher timeMatcher ) throws VizException {
   	
   		NcPaneLayout rbdPaneLayout = new NcPaneLayout(1,1);

   		if( isMultiPane() ) {
   			rbdPaneLayout = new NcPaneLayout(
   					((NcPaneLayout)currRbdPaneLayout).getRows(), 
   					((NcPaneLayout)currRbdPaneLayout).getColumns() );
   		}
   		
   		AbstractRBD<?> rbdBndl = AbstractRBD.createEmptyRbdForDisplayType( rbdType, rbdPaneLayout );
   		if( rbdBndl == null ) {
   			throw new VizException("Unsupported RBD type");
   		}
   		
   		rbdBndl.setRbdName( rbdName );
//   		rbdBndl.setPaneLayout( (NcPaneLayout)getPaneLayout() );
   		
   		if( timeMatcher != null ) {    	   		
   	   		rbdBndl.setTimeMatcher( timeMatcher );
   		}
   		
   		rbdBndl.setSelectedPaneId( (isMultiPane() ? selectedPaneId : new NcPaneID()) );
   		
   		rbdBndl.setAutoUpdate( autoUpdate );
   		rbdBndl.setGeoSyncedPanes( geoSyncPanes );
   		
   		for( int paneIndx=0 ; paneIndx<rbdPaneLayout.getNumberOfPanes() ; paneIndx++ ) {
			NcPaneID paneId = (NcPaneID)rbdPaneLayout.createPaneId(paneIndx); 
			
			PaneSelectionData paneData = paneSelectionDataMap.get( paneId.toString() );

	        PredefinedArea pArea = paneData.getArea();
 
	        // 
	        if( pArea.getSource() == AreaSource.DISPLAY_AREA ) {
	        	pArea.setAreaSource( AreaSource.INITIAL_DISPLAY_AREA.toString() );
	        	pArea.setAreaName( rbdName );
	        }
	        
	        INatlCntrsRenderableDisplay iNcRendDisp = rbdBndl.getDisplayPane( paneId );
	        
	        iNcRendDisp.setInitialArea( pArea );	        
			
			INatlCntrsDescriptor descr = (INatlCntrsDescriptor)iNcRendDisp.getDescriptor();

			// auto update is set for the AbstractRBD<?> and is also set for each descriptor
			descr.setAutoUpdate( autoUpdate );
			
   			if( descr.getResourceList() == null ) {
   	   			VizException ve = new VizException( "getResourceList is null??." );
   	   			throw ve;
   			}
   			else {
   				descr.getResourceList().clear();
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
   				descr.getResourceList().add( rscPair );                   
   			}
   			
   			// set the timeMatcher for the Descriptor (This will be the same timeMatcher
   			// for all panes.
   			descr.setTimeMatcher( timeMatcher );
			
//			rbdBndl.addDisplayPane( dispPane, paneId );	
   		}
   		
   		rbdBndl.setIsDefaultRbd( false );
   		
   		return rbdBndl;
   	}
}