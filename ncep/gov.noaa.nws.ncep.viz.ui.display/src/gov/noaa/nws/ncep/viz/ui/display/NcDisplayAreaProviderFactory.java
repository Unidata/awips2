package gov.noaa.nws.ncep.viz.ui.display;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.panes.VizDisplayPane;

import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.INcAreaProviderFactory;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

// Factory to provide the area from an INatlCntrsRenderableDisplay. Currently this is
// only displays loaded in an editor. 
// 
/**
 * Create a PredefinedArea from a renderabledisplay
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/20/13     #862     G. Hull         created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class NcDisplayAreaProviderFactory implements INcAreaProviderFactory {

	private AreaSource dispAreaSource; // either DISPLAY_AREA or INITIAL_DISPLAY_AREA
	
	public NcDisplayAreaProviderFactory() {
		
	}
	
	@Override
	public void initialize(String sourceName, String dataLoc, String configData ) throws VizException {
		
		dispAreaSource = AreaSource.createAreaSource( sourceName );
		
		// sanity check
		if( dispAreaSource != AreaSource.DISPLAY_AREA &&
			dispAreaSource != AreaSource.INITIAL_DISPLAY_AREA ) {
			System.out.println("NcDisplayAreaProviderFactory source name not matching:"+
					AreaSource.DISPLAY_AREA.toString() );					
		}
	}

	@Override
	public AreaSource getAreaSource() {
		return dispAreaSource;
	}
	
	@Override
	public List<AreaName> getAvailableAreaNames() {
		List<AreaName> curDisplayPanes = new ArrayList<AreaName>();
		
		for( AbstractEditor disp : 
			   NcDisplayMngr.getAllDisplaysOfType( NcDisplayType.NMAP_DISPLAY ) ) {
			for( IDisplayPane iPane : disp.getDisplayPanes() ) {
				if( iPane instanceof VizDisplayPane ) {
					if( iPane.getRenderableDisplay() instanceof IAreaProviderCapable ) {
						if( dispAreaSource == AreaSource.DISPLAY_AREA ) {
							curDisplayPanes.add( new AreaName( dispAreaSource,
								((IAreaProviderCapable)iPane.getRenderableDisplay()).getAreaName() ) );
						}
						else if( dispAreaSource == AreaSource.INITIAL_DISPLAY_AREA ) {
							curDisplayPanes.add( new AreaName( dispAreaSource,
									((IAreaProviderCapable)iPane.getRenderableDisplay()).getAreaName() ) );
						}
					}
				}
			}
		}
		return curDisplayPanes;
	}

	// the areaName is the name of the NcMapRenderableDisplay
	@Override
	public IGridGeometryProvider createGeomProvider( String areaName ) throws VizException {
		// get the 
		INatlCntrsRenderableDisplay disp = NcDisplayMngr.findRenderableDisplayByPaneName( 
											NcDisplayType.NMAP_DISPLAY, areaName );
		
		if( disp == null ) {
			return null;
		}
		
		return NcDisplayAreaProviderFactory.
					createPredefinedAreaFromRenderableDisplay( disp, dispAreaSource  );
		
		// if the current area is different than the initial area then 
		// the name will be the paneName with a source of DISPLAY, otherwise
		// 
//    	PredefinedArea curArea = new PredefinedArea( AreaSource.DISPLAY_AREA, 
//    			, 
//    			Double.toString( disp.getZoomLevel() ),
//    			NcDisplayType.NMAP_DISPLAY );
	    
//		try {			
//			// the name of the selected area will be the current area of the imported display
//			// unless the area has not been changed and the initial area is a predefined area
//
//			PredefinedArea initialArea = disp.getInitialArea();
//
//			if( currArea != null && initialArea != null ) { // sanity check ; should only happen for out of date Rbds.
//
//				if( initialArea.getAreaSource() != AreaSource.DISPLAY_AREA ) {
//
//					if( PredefinedArea.areAreasEqual( 
//									initialArea, (PredefinedArea)currArea ) ) {
//
//						seldAreaName = initialArea.getAreaName();					
//					}
//					seldAreaName = currArea.getProviderName();
///*				else if( pArea.getAreaSource() == AreaSource.RESOURCE_DEFINED ) {
//set the selected area but also need to update the area with the zoom level
//since it was created (when the resource was added) with the default zoom level.
//TODO : will need to change the availAreaProvidersMap to be PredefinedAreas in order to support this.
//				if( initialArea.getZoomLevel().equals( ZoomLevelStrings.SizeOfImage.toString() ) ) {						
//				}
//			}	 */			
//					}
//			}
////			addAvailAreaProvider( currArea );
//		}
//		catch ( VizException ve ) {
//			System.out.println("error getting curr area for "+dispPane.getPaneName() );
//		}
//		
//	    	
//	    	return curArea;
//	    }

//		return disp.getCurrentArea();
	}

	public static PredefinedArea createPredefinedAreaFromRenderableDisplay( 
										INatlCntrsRenderableDisplay disp, AreaSource asrc ) {
		if( asrc == AreaSource.DISPLAY_AREA ) {
			PredefinedArea pArea = PredefinedAreaFactory.createPredefinedArea( 
					new AreaName( asrc, disp.getPaneName().toString() ), 
					     disp.getDescriptor().getGridGeometry(), 
					     disp.getMapCenter(), 
					     Double.toString( disp.getZoomLevel() ) );
			return pArea;
		}
		else if( asrc == AreaSource.INITIAL_DISPLAY_AREA ) {
			return disp.getInitialArea();
		}
		else 
			return null;
	}
	@Override
	public List<VizException> getInitializationExceptions() {
		return null;
	}	
}