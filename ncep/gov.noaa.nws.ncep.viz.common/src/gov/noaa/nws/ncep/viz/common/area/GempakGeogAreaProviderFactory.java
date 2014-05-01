package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GeographicalData;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GeographicalDataReader;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GraphicsAreaCoordinates;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
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

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/15/13       #862       G. Hull     Created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class GempakGeogAreaProviderFactory implements INcAreaProviderFactory {

	public static class GempakGeogAreaProvider implements IGridGeometryProvider {

		private AreaName geogName;

		private GeneralGridGeometry gridGeom;
			
		public GempakGeogAreaProvider( AreaName name, GeneralGridGeometry geom ) {
			geogName = name;
			gridGeom = geom;				
		}
		
		@Override
		public GeneralGridGeometry getGridGeometry() {
			return gridGeom;
		}
		
		@Override
		public AreaSource getSource() {
			return geogName.getSource();
		}

		@Override
		public String getProviderName() {
			return geogName.getName();
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

	private String geogFileName=null;
	private String stnFileName=null; // not used but have to pass to 
	
	// may be the GEMPAK_GEOG_AREA_NAME or GEMPAK_GEOG_AREA_CODE
	private AreaSource sourceName;
	
	// a map from all avail areanames to the geog code 
	private Map<String,String> areaToGeogCodeMap = new HashMap<String,String>();
	

	private List<VizException> badGeogAreas = new ArrayList<VizException>();
	
	private ILocalizationFileObserver locObs = new ILocalizationFileObserver() {
		
		@Override
		public void fileUpdated(FileUpdatedMessage message) {
			System.out.println(message.getFileName() + " updated");
		}
	};

	public GempakGeogAreaProviderFactory() {		
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
						NcPathConstants.GEOG_TBL +"). Using the default.");
//			badGeogAreas.add( new VizException("") );
		}
		else {
			geogFileName = geogLFile.getFile().getAbsolutePath();
			
			geogLFile.addFileUpdatedObserver( locObs );			
		}
				
		GeographicalDataReader geogDataReader = new GeographicalDataReader(geogFileName);
		
		try {			
			List<GeographicalData> 	geogDataList = geogDataReader.getGeographicalData();
			
			for( GeographicalData geogData : geogDataList) {				
				AreaName geogAreaName = getAreaNameFromTableEntry( geogData );
				try {			
					areaToGeogCodeMap.put( geogAreaName.getName(), geogData.getGeogCode().trim() );
						
					createGeomProvider( geogAreaName.getName( ) );					
				}
				catch ( VizException ve ) {
					badGeogAreas.add( ve );
					areaToGeogCodeMap.remove( geogAreaName.getName() );
				}
			}

		} catch (JAXBException e) {
		}

		stnFileName = sfcLFile.getName();
			//instance = new GempakGeogAreaProviderFactory( geogLFile.getName() );
	}
		
	@Override
	public AreaSource getAreaSource() {
		return sourceName;//"GEMPAK_GEOG_AREA";
	}

	@Override
	public List<AreaName> getAvailableAreaNames() {
		List<AreaName> retList = new ArrayList<AreaName>( areaToGeogCodeMap.keySet().size() );
		for( String an : areaToGeogCodeMap.keySet() ) {
			retList.add( new AreaName( getAreaSource(), an ) );
		}
		return retList;
	}

	
	@Override
	public IGridGeometryProvider createGeomProvider( String areaName ) throws VizException {

		if( !areaToGeogCodeMap.containsKey( areaName ) ) {
			throw new VizException( "Unrecognized areaName "+ areaName+". couldn't find entry in the geog tbl");			
		}
		String geogCode = areaToGeogCodeMap.get( areaName );

		// CustomProjectionServiceImpl handles all GAREA syntax but
		// we only are interested in the GEOG table entries, check to make sure
		// this is a valid GEOG area
		GraphicsAreaCoordinates gareaCoords = new GraphicsAreaCoordinates( geogCode );

		gareaCoords.setGeogFileName( geogFileName );
		gareaCoords.setStationFileName( stnFileName ); 
//		if( !gareaCoords.isValidGeogName() ) {
//			throw new VizException("Area, "+ geogCode+", is not a valid Geog Table Entry");
//		}
		boolean areaValid = gareaCoords.parseGraphicsAreaString( geogCode ); 

		if( !areaValid ) {
			throw new VizException( "Area, "+ geogCode+", is not valid.");
		}
		
		CustomProjectionServiceImpl customProj = 
	    	new CustomProjectionServiceImpl( "", // gareaCoords.getMapProjectionString(), 
	    			geogCode, geogFileName, stnFileName );
	
//		customProj.isGempakGraphicsAreaStringValid();
		
		CoordinateReferenceSystem crs = customProj.getCoordinateReferenceSystem();
		GeneralGridGeometry gridGeom  = null;
		
		try {
		if( customProj.isGraphicAreaTextWithValidGeogName() ) {
			gridGeom = MapDescriptor.createGridGeometry( crs, 
					new Coordinate( customProj.getLowerLeftLon(), customProj.getLowerLeftLat() ),
					new Coordinate( customProj.getUpperRightLon(), customProj.getUpperRightLat() ) );	            						
		}
//		if( customProj.isCenterDeltaLatLonValuesValid() ) {
//			gridGeom = MapDescriptor.createGridGeometry( crs, 
//					new Coordinate( customProj.getLowerLeftLon(), customProj.getLowerLeftLat() ),
//					new Coordinate( customProj.getUpperRightLon(), customProj.getUpperRightLat() ) );	            			
//		}
//		else if( customProj.isLowerLeftAndUpperRightLatLonValuesValid() ) {
//			gridGeom = MapDescriptor.createGridGeometry( crs, 
//				new Coordinate( customProj.getLowerLeftLon(), customProj.getLowerLeftLat() ),
//				new Coordinate( customProj.getUpperRightLon(), customProj.getUpperRightLat() ) );
//		}
		else {
			throw new VizException( "Area, "+ geogCode+", is not valid as bounding box or center/range.");
		}
		}
		catch ( VizException ve ) {
			throw new VizException( "Area, "+ geogCode+", is not valid:"+ve.getMessage() );
		}
		GempakGeogAreaProvider geogEntry = 
			new GempakGeogAreaProvider( new AreaName( sourceName, areaName), gridGeom );
		
		return geogEntry;
	}
	
	private AreaName getAreaNameFromTableEntry( GeographicalData geoData ) {
		if( sourceName == AreaSource.getAreaSource("GEMPAK_GEOG_AREA_NAME") ) {
			return new AreaName( sourceName, geoData.getGeogAreaName() );				
		}
		else {
			return new AreaName( sourceName, geoData.getGeogCode() );
		}
	}

	@Override
	public List<VizException> getInitializationExceptions() {
		return badGeogAreas;
	}

//	@Override
//	public void setInitializationData(IConfigurationElement config,
//			String propertyName, Object data) throws CoreException {
//		System.out.println("setInitializationData called with propertyName "+
//				propertyName+ " = "+data.toString() );
//	}
}
