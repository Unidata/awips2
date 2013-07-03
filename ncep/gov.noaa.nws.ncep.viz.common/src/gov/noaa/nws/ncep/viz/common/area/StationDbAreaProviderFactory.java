package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 *  generate a PredefinedArea (IGridGeometryProvider) from a station in common_obs_spatial.
 *  
 *  TODO : allow user to set/override the projection and/or the extents.
 *  
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/20/14       #862      Greg Hull    Created.
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class StationDbAreaProviderFactory implements INcAreaProviderFactory {

	public static class StationDbAreaProvider implements IGridGeometryProvider {

		// the stnid but could also represent the stnnum or name with a small code 
		// change. (see the GempakeAreaProviderFactory)
		private AreaName stnIdName;
		
		private String catType;
		private Coordinate stnLoc;
		private GeneralGridGeometry gridGeom;
		
		public StationDbAreaProvider( AreaName sid, String cat ) throws VizException {
			stnIdName = sid;
			catType = cat;
			
			String sql = "SELECT AsBinary("+ (catType.equals("22") ? "upperairgeom" : "the_geom" ) + 
				") FROM "+dbTbl +" WHERE catalogtype = "+
			   catType + " and stationid = '"+ stnIdName.getName() + "'";
			   
			try {
				stnLoc = getCoordinate( DirectDbQuery.executeQuery( sql, dbName, DirectDbQuery.QueryLanguage.SQL) );
	    		gridGeom = null;

	    		// No projection is given so we will create one that is meaningful.
	    		Coordinate ll = new Coordinate( stnLoc.x-areaWidth, stnLoc.y-areaHeight );
	    		Coordinate ur = new Coordinate( stnLoc.x+ areaWidth, stnLoc.y+areaHeight );

	    		gridGeom = MapDescriptor.createGridGeometry( 
	    				getDefaultCRSForLoc( stnLoc ), ll, ur );
	    		
	        } catch (Exception e) {
	        	throw new VizException( "Error creating geometry for area "+stnIdName+ ": "+e.getMessage() );
	        }
		}
		
		@Override
		public GeneralGridGeometry getGridGeometry() {
			return gridGeom;
		}
		
		@Override
		public AreaSource getSource() {
			return stnIdName.getSource();
		}

		@Override
		public String getProviderName() {
			return stnIdName.getName();
		}
		
		//  compute the map center.
		@Override
		public double[] getMapCenter() {	
			return ( stnLoc == null ? null : new double[] { stnLoc.x, stnLoc.y } );
		}

		@Override
		public String getZoomLevel() {
			return "1.0";
		}

		@Override
		public void setZoomLevel(String zl) {			
		}
	}
	
	// hardcoded as done in gempak. TODO : change to allow user to set this.
	
	private static String dbName = "metadata"; // could set this from the location string
	private static String dbTbl  = "common_obs_spatial"; // set (sanity check) from the location string

	// a map from the areaname to a (possibly null if not created) area provider
	private Map<String,IGridGeometryProvider> stnAreasMap = new HashMap<String,IGridGeometryProvider>();
	
	private String catTypeStr;
		
	private AreaSource sourceName;

	private List<VizException> badStnAreas = new ArrayList<VizException>();
	
	// TODO allow this to be changed so user can configure for different projections.
	private static String dfltProjection = "Stereographic";

	// TODO allow user to override/configure the extents
	private static Double areaWidth=7.0; // degrees (from gempak)
	private static Double areaHeight=4.0; // degrees
	
	// default to Stereographic
	public static String[] supportedProjections = {
		"Stereographic",
		"Stereographic_North_Pole",
		"Mercator_2SP",
		"Equidistant_Cylindrical",
		"Lambert_Conformal_Conic_1SP"
		//"Lambert_Azimuthal_Equal_Area"		
	};
	
	public StationDbAreaProviderFactory() {		
	}

	@Override
	public AreaSource getAreaSource() {
		return sourceName;
	}
	
	// location is the common_obs_spatial db table and configData is the 
	// value of the catalogtype column
	@Override
	public void initialize( String srcName, String dataLoc, String configData ) throws VizException {
		
		try {
			catTypeStr = Integer.toString( Integer.parseInt( configData ) );
		}
		catch ( NumberFormatException e ){
			throw new VizException( "Error initializing Area Source, "+srcName+": couldn't parse "+
					"the catalogtype for the common_obs_spatial db table " );
		}

		sourceName = AreaSource.createAreaSource( srcName );

		//TODO : only using this class and not the StaticDataProvider because the
		// underlying code for GraphicsAreaCoordinates does.		

		String sql = "SELECT stationid FROM "+dbTbl+" WHERE catalogtype = "+catTypeStr;
		
		try {
            List<Object[]> stnList = DirectDbQuery.executeQuery( sql, dbName, DirectDbQuery.QueryLanguage.SQL);

            // We could validate the stations by calling createGeomProvider but there isn't anything 
            // we can do to fix the table and it will take time so just wait to validate til its needed.
            for( Object[] stn : stnList ) {
            	AreaName stnAreaName = new AreaName( getAreaSource(), (String)stn[0] );
//            	try {			
            		stnAreasMap.put( (String)stn[0]/*stnAreaName.toString()*/, null );
//            		createGeomProvider( stnAreaName.getName( ) );
////            		stnAreasMap.put( stnAreaName.toString(), null );
//            	}
//            	catch ( VizException ve ) {
//            		badsfstnAreas.add( ve );
//            		stnAreasMap.remove( stnAreaName.toString() );
//            	}
            }		
        } catch (Exception e) {
        	throw new VizException( e );
        }

        System.out.println("validated "+stnAreasMap.keySet().size()+ " stations for cat "+catTypeStr );
	}
		
	@Override
	public List<AreaName> getAvailableAreaNames() {
		List<AreaName> retList = new ArrayList<AreaName>( stnAreasMap.keySet().size() );
		for( String an : stnAreasMap.keySet() ) {
			retList.add( new AreaName( getAreaSource(), an ) );
		}
		return retList;
	}
	
	@Override
	public IGridGeometryProvider createGeomProvider( String areaName ) throws VizException {
		if( !stnAreasMap.containsKey( areaName ) ) {
			throw new VizException("areaName, "+ areaName+", not found by source, "+ getAreaSource().toString() );
		}
		
		IGridGeometryProvider areaProv = stnAreasMap.get( areaName );
 
		if( areaProv == null ) {
			areaProv = new StationDbAreaProvider( new AreaName( getAreaSource(), areaName ), catTypeStr );
			stnAreasMap.put( areaName, areaProv );
		}
		 
		return areaProv;
	}
	
    private static Coordinate getCoordinate(List<Object[]> data) throws VizException {
        if (data == null || data.isEmpty()) {
            return null;
        }
        Object[] dataArray = data.get(0);
        if (dataArray == null || dataArray.length != 1) {
            return null;
        }
        byte[] bytes = (byte[]) dataArray[0];
        WKBReader reader = new WKBReader();
        Geometry geo = null;
        try {
            geo = reader.read(bytes);
            return geo.getCoordinate();
        } catch (ParseException e) {
            throw new VizException(e);
        }
    }

    // TODO : not called and for now only "Stereographic" is used.
    // Note will need to recreate gridgeom if already created.
    public static void setDefaultProjection( String proj ) {
    	if( !Arrays.asList( supportedProjections ).contains( proj ) ) {
    		System.out.println(proj+" is not a supported proj for StationsDbAreas");
    		return;
    	}
    	dfltProjection = proj;
    }
    
    public static CoordinateReferenceSystem getDefaultCRSForLoc( Coordinate loc ) {
    	return getDefaultCRSForLoc( dfltProjection, loc );
    }
    
    public static CoordinateReferenceSystem getDefaultCRSForLoc( String proj, Coordinate loc ) {
    	if( !Arrays.asList( supportedProjections ).contains( proj ) ) {
    		System.out.println(proj+" is not a supported proj for StationsDbAreas");
    		return null;
    	}

    	if( proj.equals( "Stereographic" ) ) {
    		return MapUtil.constructStereographic(
				MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, loc.y, loc.x );
    	}
    	else if( proj.equals( "Stereographic_North_Pole" ) ) {
    		return MapUtil.constructNorthPolarStereo(
    				MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, loc.y, loc.x );
    	}
    	else if( proj.equals( "Mercator_2SP" ) ) {
    		return MapUtil.constructMercator( 
    				MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, loc.y, loc.x );
    	}
    	else if( proj.equals( "Equidistant_Cylindrical" ) ) {
    		return MapUtil.constructEquidistantCylindrical(  
    				MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, loc.x, loc.y );
    	}
    	else if( proj.equals( "Lambert_Conformal_Conic_1SP" ) ) {
    		return MapUtil.constructLambertConformal( 
    				MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, 
    				loc.y, loc.y, loc.x );
    	}
    	else {
    		return null;
    	}
//        try {
//        	DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();
//            ParameterValueGroup parameters = dmtFactory .getDefaultParameters( dfltProjection );
//
//            // check for and replace central_meridian and standard_parallel parameters
//            parameters.parameter("standard_parallel_1").setValue(stdParallel);
//            parameters.parameter("central_meridian").setValue(centralMeridian);
// determine name??
//            return constructProjection(name, parameters);
//        } catch (Exception e) {
//            return null;
//        }
    }
    
	@Override
	public List<VizException> getInitializationExceptions() {
		return null;
	}

}
