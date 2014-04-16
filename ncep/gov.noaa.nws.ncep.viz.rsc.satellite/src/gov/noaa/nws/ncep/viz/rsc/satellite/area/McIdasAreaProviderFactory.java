package gov.noaa.nws.ncep.viz.rsc.satellite.area;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.BoundaryTool;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.vividsolutions.jts.geom.Polygon;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.INcAreaProviderFactory;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

public class McIdasAreaProviderFactory implements INcAreaProviderFactory {

	public static class McIdasAreaProvider implements IGridGeometryProvider {

		private GeneralGridGeometry gridGeom;
		private String areaName;
		
		public McIdasAreaProvider( String a) {
			gridGeom = null;
			areaName = a;
		}

		@Override
		public GeneralGridGeometry getGridGeometry() {
			if( gridGeom != null ) {
				return gridGeom;
			}

			try {			
				String[] satAndArea = areaName.split( File.separator );
				if( satAndArea.length != 2 || 
						satAndArea[0].isEmpty() || 
						satAndArea[1].isEmpty() ) { 
					throw new VizException("invalid mcidas area name. Expecting <satName>/<areaName>.");
				}
			
				HashMap<String, RequestConstraint> reqConstraints = new HashMap<String, RequestConstraint>();
				reqConstraints.put( "pluginName", new RequestConstraint( pluginName) );		
				reqConstraints.put( "satelliteName", new RequestConstraint( satAndArea[0]) );		
				reqConstraints.put( "areaName", new RequestConstraint( satAndArea[1]) );
				
				LayerProperty prop = new LayerProperty();
				prop.setDesiredProduct(ResourceType.PLAN_VIEW);
				prop.setEntryQueryParameters( reqConstraints, false);
				prop.setNumberOfImages(1); // just need 1 record to get the coverage
				
				String script = ScriptCreator.createScript(prop);
		
				Object[] satRecList = Connector.getInstance().connect( script, null, 10000 );
		
				// NOTE: It would be nice to query the mcidas_area_names and/or mcidas_spatial
				// directly so that we don't have to depend on data being in the db, but
				// this is fine for now.
				//
				if( satRecList == null || satRecList.length == 0 || 
						!(satRecList[0] instanceof McidasRecord) ) {
					throw new VizException("No data for areaName, "+areaName );
				}

				McidasRecord satRec = (McidasRecord)satRecList[0];

	            if( satRec.getProjection().equalsIgnoreCase("STR") ||
	            	satRec.getProjection().equalsIgnoreCase("MER") ||
	            	satRec.getProjection().equalsIgnoreCase("LCC")) {
	                	
	            	// for remapped projections such as MER, LCC, STR
	            	gridGeom = MapUtil.getGridGeometry( satRec.getSpatialObject() );
	            } 
	            else {
	        		McidasMapCoverage coverage = satRec.getCoverage();
	        		
	        	    GeneralEnvelope env = new GeneralEnvelope(2);
	        	    env.setCoordinateReferenceSystem( satRec.getCoverage().getCrs() );
	        	    
	        	    int minX = coverage.getUpperLeftElement();
	        	    int maxX = coverage.getUpperLeftElement() + ( coverage.getNx() * coverage.getElementRes() );
	        	    int minY = coverage.getUpperLeftLine() + ( coverage.getNy() * coverage.getLineRes() );
	        	    minY = -minY;
	        	    int maxY = -1 * coverage.getUpperLeftLine();
	        	    env.setRange(0, minX, maxX);
	        	    env.setRange(1, minY, maxY);
	        	    
	        	    gridGeom = new GridGeometry2D(
	        	    	new GeneralGridEnvelope(new int[] {
	                           0, 0 }, new int[] { coverage.getNx(), 
	        	    							   coverage.getNy() }, false), env);
	            }
			}
			catch (VizException e) {
//				throw new VizException("Could not query a McIdasRecord to get the image geometry:"+e.getMessage());
				System.out.println("Could not query a McIdasRecord to get the image geometry:"+e.getMessage());
			}

			// ??? return something meaningful???
			return gridGeom;		
		}

		@Override
		public AreaSource getSource() {
			return sourceName;
		}

		@Override
		public String getProviderName() {
			return areaName;
		}

		@Override
		public double[] getMapCenter() {
			return null;
		}

		@Override
		public String getZoomLevel() {
			return Double.toString( 1.0 ); //IGridGeometryProvider.ZoomLevelStrings.FitToScreen.toString();
		}

		@Override
		public void setZoomLevel(String zl) {
		}
		
	}
	
	private static AreaSource sourceName;

	private static String pluginName="mcidas"; // TODO : set from ext point location parameter
    // TODO use the location string to set the names of the db columns to query for the area.
	// hardcoded for now since the method for querying the area may change if we can 
	// link the mcidas_area_names table to the mcidas_spatial table.
	//	
	private Map<String,McIdasAreaProvider> availAreasMap = new HashMap<String, McIdasAreaProvider>();

	@Override
	public AreaSource getAreaSource() {
		return sourceName;//AreaSource.MCIDAS_AREA_NAME.toString();
	}


	@Override
	public List<VizException> getInitializationExceptions() {
		return null;
	}

	@Override
	public List<AreaName> getAvailableAreaNames() {
		List<AreaName> areaNames = new ArrayList<AreaName>( availAreasMap.keySet().size() );
		
		for( String aname : availAreasMap.keySet() ) {
			areaNames.add( new AreaName( getAreaSource(), aname ) );
		}
		return areaNames;
	}
	
	public McIdasAreaProviderFactory() {
		
	}

	@Override
	public void initialize( String srcName, String dataLoc, String configData ) throws VizException {
		
		sourceName = AreaSource.createImagaeBasedAreaSource( srcName );
		
		HashMap<String, RequestConstraint> reqConstraints = new HashMap<String, RequestConstraint>();
		reqConstraints.put( "pluginName", new RequestConstraint( pluginName) );
		
        try {            
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints( reqConstraints );
//                request.addRequestField("coverage");
            request.addRequestField("satelliteName");
            request.addRequestField("areaName");           
            request.setDistinct(true);
            DbQueryResponse response = (DbQueryResponse) ThriftClient.sendRequest(request);

            for( Map<String, Object> result : response.getResults() )  {
//            	AreaName satAreaName = new AreaName( getAreaSource(),
            	String satAreaName = result.get("satelliteName")+File.separator+result.get("areaName");
            	if( !availAreasMap.containsKey( satAreaName ) ) {
                	availAreasMap.put( satAreaName, new McIdasAreaProvider( satAreaName ) ); // get geom later when requested.            		
            	}
            }
        } catch (Exception e) {
        	throw new VizException( e );
        }			
	}

	@Override
	public IGridGeometryProvider createGeomProvider(String areaName)
			throws VizException {
		
		if( !availAreasMap.containsKey( areaName ) ) {
		// ???? what should we do here
			availAreasMap.put( areaName, new McIdasAreaProvider( areaName ) );
		}
		
		return availAreasMap.get( areaName );
	}	
}