package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.viz.common.area.AreaMenus.AreaMenuItem;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

public class AreaMenusMngr {
		
	// a map from the submenu item (or "") to a list of the AreaMenuItems under the submenu
	private Map<String,List<AreaMenuItem>> areaMenuItemsMap = null;
	
	private static LocalizationFile menusLocFile = null; // save to add/remove observer
	
	private static Boolean showDisplayAreas = false; // include other Displays' areas in Areas main menu 
	private static Boolean showImageBasedResourceAreas = true;
	
	private static AreaMenusMngr instance;
	
	public static AreaMenusMngr getInstance() {
		if( instance == null ) {
			instance = new AreaMenusMngr();
		}
		return instance;
	}
		
	private AreaMenusMngr( ) {
				
		try {
			readMenusFile();
		} catch (VizException e) {
			System.out.println( e.getMessage() );
			return;
		}
	}

	// areaMenuNames has all names But only return valid area names. (if user creates
	// the area after
	public Map<String,List<AreaMenuItem>> getPredefinedAreasForMenus( ) {
	
		return new HashMap<String,List<AreaMenuItem>>( areaMenuItemsMap );
	}

	private void readMenusFile() throws VizException {
		if( areaMenuItemsMap != null ) {
			return;
		}
		try {
			menusLocFile = NcPathManager.getInstance().getStaticLocalizationFile( 
					NcPathConstants.AREA_MENUS_FILE );
			
			if( menusLocFile == null ) {
				createDefaultMenus();
				return;
			}

			// add this before parsing the file in case there is an error with the file.
			menusLocFile.addFileUpdatedObserver( new ILocalizationFileObserver() {			
				@Override
				public void fileUpdated(FileUpdatedMessage message) {
					if( menusLocFile != null ) {
						menusLocFile.removeFileUpdatedObserver( this );
					}

					synchronized ( this ) {
						TreeMap<String,List<AreaMenuItem>> savedAreaMenuItemsMap = 
							new TreeMap<String,List<AreaMenuItem>>( areaMenuItemsMap );

						areaMenuItemsMap = null;
						try {
							readMenusFile();
						} catch (VizException e) {
							areaMenuItemsMap = savedAreaMenuItemsMap;
							e.printStackTrace();
						}
					}
				}
			});

			// reference the objects in the unmarshalled list of menu items
			// NOTE: for now there isn't a way to order the sub menus. 
			// 
			areaMenuItemsMap = new TreeMap<String,List<AreaMenuItem>>();
			
			AreaMenus areaMenuItems = SerializationUtil.jaxbUnmarshalFromXmlFile( 
					AreaMenus.class, menusLocFile.getFile() );
				
			showDisplayAreas = areaMenuItems.getShowDisplayAreas();
			showImageBasedResourceAreas= areaMenuItems.getShowImageBasedResourceAreas();
			
			// validate and add to the map
			for( AreaMenuItem ami : areaMenuItems.getAreaMenuItems() ) {				
				try {
					String srcStr = ami.getSource();
					if( srcStr == null || srcStr.isEmpty() ||
							AreaSource.getAreaSource(srcStr) == AreaSource.UNKNOWN_SOURCE ) {
						throw new VizException( "Source is null, empty or unknown: "+
								(srcStr == null ? "null" : srcStr ));
					}
					AreaSource areaSource = AreaSource.getAreaSource(srcStr);
					INcAreaProviderFactory areaProv = 
						NcAreaProviderMngr.getSourceProviderFactory( areaSource );
					if( areaProv == null ) {
						throw new VizException( srcStr + ", has no area provider???");
					}
					String nameStr = ami.getAreaName();
					if( nameStr == null || nameStr.isEmpty() ) {
						throw new VizException("areaName is null or empty."	);
					}
										
					String subMenuName = ami.getSubMenuName();
					List<AreaMenuItem> menuItemsList = areaMenuItemsMap.get( subMenuName );
					
					if( menuItemsList == null ) {
						menuItemsList = new ArrayList<AreaMenuItem>();
						areaMenuItemsMap.put( subMenuName, menuItemsList );
					}
					
					List<AreaName> areaNames = new ArrayList<AreaName>();
					
					if( nameStr.equalsIgnoreCase("All") ) {
						if( ami.getSubMenuName().isEmpty() ) {
							//throw new VizException("");
						}
						areaNames = areaProv.getAvailableAreaNames();						
					}
					else {
						areaNames.add( new AreaName( areaSource, nameStr ) );
					}
					
					for( AreaName areaName : areaNames ) {
						// NOTE: this doesn't catch all invalid areas since some sources (ie common_obs_spatial)
						// don't validate all of the areas (db entries) on start up.
						if( !areaProv.getAvailableAreaNames().contains( areaName ) ) {
							throw new VizException( nameStr+ " is not a valid area for source, "+srcStr );
						}

						if( menuItemsList.contains( ami ) ) {
							throw new VizException( "AreaName," + areaName.toString()+", is already in the map.");
						}
						
						// add to the map of valid menu items
						//
						menuItemsList.add( ami );
					}					
				}
				catch (VizException ve ) {
					System.out.println("Error reading " + menusLocFile.getName()+ " file: "+ve.getMessage() );						
				}
			}				
		} 
		catch (SerializationException e) {
			throw new VizException( e );			
		} 
	}
	
	// all of the Predefined Areas.
	private void createDefaultMenus() {
		INcAreaProviderFactory pAreaProv = 
			NcAreaProviderMngr.getSourceProviderFactory( AreaSource.PREDEFINED_AREA );
		
		if( pAreaProv != null ) {
			List<AreaMenuItem> menuItemsList = new ArrayList<AreaMenuItem>();
			areaMenuItemsMap.put( "", menuItemsList );

			for( AreaName aName : pAreaProv.getAvailableAreaNames() ) {
				AreaMenuItem ami = new AreaMenuItem();
				ami.setMenuName( aName.getName() );
				ami.setSubMenuName( "" ); 
				ami.setAreaName( aName.getName() );
				ami.setSource( aName.getSource().toString() );

				menuItemsList.add( ami );
			}
		}		
	}

	public Boolean showDisplayAreas() {
		return showDisplayAreas;
	}

	public Boolean showImageBasedResourceAreas() {
		return showImageBasedResourceAreas;
	}

	// to sort the list of area names. The default goes first
//	public static class AreaNamesComparator implements Comparator<String> {
//		private NcDisplayType dispType=NcDisplayType.NMAP_DISPLAY;
//		
//		public AreaNamesComparator( NcDisplayType dt ) {
//			dispType = dt;
//		}
//		
//		@Override
//		public int compare(String a1, String a2) {
//			if( a1.equals(a2) ) {
//				return 0;
//			}
//			if( a1.equals( dispType.getDefaultMap() ) ) {
//				return -1;
//			}
//			if( a2.equals( dispType.getDefaultMap() ) ) {
//				return 1;
//			}
//
//			int a1menuIndx = (areaMenuNames == null ? 999 : (areaMenuNames.contains(a1) ? areaMenuNames.indexOf(a1) : 999) );
//			int a2menuIndx = (areaMenuNames == null ? 999 : (areaMenuNames.contains(a2) ? areaMenuNames.indexOf(a2) : 999) );
//			
//			if( a1menuIndx == a2menuIndx ) { // ie both -1
//				return a1.compareTo( a2 );
//			}
//			return (a1menuIndx < a2menuIndx ? -1 : 1 );
//		}
//	} 

}
