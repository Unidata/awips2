package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GraphicsAreaCoordinates;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.station.Station;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.station.StationDataReader;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.GempakGeogAreaProviderFactory.GempakGeogAreaProvider;
import gov.noaa.nws.ncep.viz.common.customprojection.CustomProjectionServiceImpl;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

public class GempakSfcStnAreaProviderFactory implements INcAreaProviderFactory {

	public static class GempakSfcStnAreaProvider implements IGridGeometryProvider {

		// the stnid but could also represent the stnnum or name with a small code 
		// change. (see the GempakeAreaProviderFactory)
		private AreaName sfstnName;

		private GeneralGridGeometry gridGeom;
			
		public GempakSfcStnAreaProvider( AreaName name, GeneralGridGeometry geom ) {
			sfstnName = name;
			gridGeom = geom;				
		}
		
		@Override
		public GeneralGridGeometry getGridGeometry() {
			return gridGeom;
		}
		
		@Override
		public AreaSource getSource() {
			return sfstnName.getSource();
		}

		@Override
		public String getProviderName() {
			return sfstnName.getName();
		}
		
		//  compute the map center.
		@Override
		public double[] getMapCenter() {	
			return null;
		}

		@Override
		public String getZoomLevel() {
			return "1.0";
		}

		@Override
		public void setZoomLevel(String zl) {			
		}
	}

	private String geogFileName=null; // not used but have to pass to
	private String stnFileName=null; 
	
	// TODO : change to allow referencing by the number or id.
	private AreaSource sourceName;
	
	private Map<String,String> areaToSfstnIdMap = new HashMap<String,String>();
	

	private List<VizException> badsfstnAreas = new ArrayList<VizException>();
	
	private ILocalizationFileObserver locObs = new ILocalizationFileObserver() {
		
		@Override
		public void fileUpdated(FileUpdatedMessage message) {
			System.out.println(message.getFileName() + " updated");
		}
	};

	public GempakSfcStnAreaProviderFactory() {		
	}
	
	@Override
	public void initialize( String srcName, String dataLoc, String configData ) throws VizException {
		
		sourceName = AreaSource.createAreaSource( srcName );

		LocalizationFile geogLFile = NcPathManager.getInstance().getStaticLocalizationFile( 
					NcPathConstants.GEOG_TBL );
		LocalizationFile sfcLFile = NcPathManager.getInstance().getStaticLocalizationFile( 
					NcPathConstants.SFSTNS_TBL );

		if( geogLFile == null || sfcLFile == null ) {
				// GraphicsAreaCoordinates has its own version of the file.
			System.out.println("Unable to find localization file for the geog table ("+
						NcPathConstants.SFSTNS_TBL +"). Using the default.");
		}
		else {
			geogFileName = geogLFile.getFile().getAbsolutePath();
			stnFileName = sfcLFile.getFile().getAbsolutePath();
			
			sfcLFile.addFileUpdatedObserver( locObs );			
		}

		//TODO : only using this class and not the StaticDataProvider because the
		// underlying code for GraphicsAreaCoordinates does.		
		StationDataReader stnDataReader = new StationDataReader( stnFileName );
		List<Station> stationList = null;
		try {
			stationList = stnDataReader.getStationData();
		} catch (JAXBException e) {
			throw new VizException( e );
		}

		// would be nice to validate but it takes too long
		for( Station stn : stationList ) {
			 
			AreaName geogAreaName = getAreaNameFromTableEntry( stn );
//			try {			
				areaToSfstnIdMap.put( geogAreaName.getName(), stn.getStid().trim() );
//						
//				createGeomProvider( geogAreaName.getName( ) );					
//			}
//			catch ( VizException ve ) {
//				badsfstnAreas.add( ve );
//				areaToSfstnIdMap.remove( geogAreaName.getName() );
//			}
		}
	}
		
	@Override
	public AreaSource getAreaSource() {
		return sourceName;//"GEMPAK_GEOG_AREA";
	}

	@Override
	public List<AreaName> getAvailableAreaNames() {
		List<AreaName> retList = new ArrayList<AreaName>( areaToSfstnIdMap.keySet().size() );
		for( String an : areaToSfstnIdMap.keySet() ) {
			retList.add( new AreaName( getAreaSource(), an ) );
		}
		return retList;
	}

	
	@Override
	public IGridGeometryProvider createGeomProvider( String areaName ) throws VizException {

		if( !areaToSfstnIdMap.containsKey( areaName ) ) {
			throw new VizException( "Unrecognized areaName "+ areaName+". couldn't find entry in the geog tbl");			
		}
		String sfstnid = areaToSfstnIdMap.get( areaName );

		// CustomProjectionServiceImpl handles all GAREA syntax but
		// we only are interested in the GEOG table entries, check to make sure
		// this is a valid GEOG area
		GraphicsAreaCoordinates gareaCoords = new GraphicsAreaCoordinates( sfstnid );
		gareaCoords.setGeogFileName( geogFileName );
		gareaCoords.setStationFileName( stnFileName );
		
		boolean areaValid = gareaCoords.parseGraphicsAreaString( sfstnid ); 

		if( !areaValid ) {
			throw new VizException( "Area, "+ sfstnid+", is not valid.");
		}
		
		CustomProjectionServiceImpl customProj = 
	    	new CustomProjectionServiceImpl( "", // gareaCoords.getMapProjectionString(), 
	    			sfstnid, geogFileName, stnFileName );
	
//		customProj.isGempakGraphicsAreaStringValid();
	
		CoordinateReferenceSystem crs = customProj.getCoordinateReferenceSystem();
		GeneralGridGeometry gridGeom = null;
		
			gridGeom = MapDescriptor.createGridGeometry( crs, 
					new Coordinate( customProj.getLowerLeftLon(), customProj.getLowerLeftLat() ),
					new Coordinate( customProj.getUpperRightLon(), customProj.getUpperRightLat() ) );	            						
//		else {
//			throw new VizException( "Area, "+ sfstnid+", is not valid as bounding box or center/range.");
//		}

		GempakGeogAreaProvider geogEntry = 
			new GempakGeogAreaProvider( new AreaName( sourceName, areaName), gridGeom );
		
		return geogEntry;
	}
	
	private AreaName getAreaNameFromTableEntry( Station stn  ) {
		return new AreaName( sourceName, stn.getStid() );
		// TODO : if we want to support referencing with stnum or name
//		if( sourceName == AreaSource.getAreaSource("GEMPAK_SFC_STN_ID") ) {
//			return new AreaName( sourceName, stn.getStnnum() );				
//		}
//		else {
//			return new AreaName( sourceName, stn.getStnname );
//		}
	}

	@Override
	public List<VizException> getInitializationExceptions() {
		return null;
	}

}
