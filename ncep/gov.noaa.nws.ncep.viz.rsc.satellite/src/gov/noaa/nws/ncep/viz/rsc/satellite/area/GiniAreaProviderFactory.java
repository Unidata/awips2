package gov.noaa.nws.ncep.viz.rsc.satellite.area;

import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.INcAreaProviderFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.ResourceType;

public class GiniAreaProviderFactory implements INcAreaProviderFactory {

	public static class GiniAreaProvider implements IGridGeometryProvider {

		private GeneralGridGeometry gridGeom;
		private String areaName;
		
		public GiniAreaProvider( String a) {
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
					throw new VizException("invalid gini area name. Expecting <creatingEntity>/<sectorid>.");
				}

				HashMap<String, RequestConstraint> reqConstraints = new HashMap<String, RequestConstraint>();
				reqConstraints.put( "pluginName", new RequestConstraint( pluginName) );		
				reqConstraints.put( "creatingEntity", new RequestConstraint( satAndArea[0]) );		
				reqConstraints.put( "sectorID", new RequestConstraint( satAndArea[1]) );
				
		        DbQueryRequest request = new DbQueryRequest();
		        request.setConstraints(reqConstraints);
		      
		        DbQueryResponse response = (DbQueryResponse) ThriftClient.sendRequest(request);
		
				Object[] satRecList =  ((Map<String, Object>)response.getResults().get(0)).values().toArray();
		
				// NOTE: It would be nice to query the mcidas_area_names and/or mcidas_spatial
				// directly so that we don't have to depend on data being in the db, but
				// this is fine for now.
				//
				if( satRecList == null || satRecList.length == 0 || 
						!(satRecList[0] instanceof SatelliteRecord) ) {
					throw new VizException("No data for areaName, "+areaName );
				}

				SatelliteRecord satRec = (SatelliteRecord)satRecList[0];
						
				int proj = satRec.getCoverage().getProjection();

				if( proj == 1 || proj == 3 || proj == 5 ) { // MER, LCC or STR 
	                // for remapped projections such as MER, LCC, STR
					gridGeom = satRec.getGridGeometry();
				}
				else {
					System.out.println("Unable to get Coverage for projection "+ proj+ "." );							
				}
						
			} catch (VizException e) {
				System.out.println("Could not query a SatelliteRecord to get the image geometry:"+e.getMessage());
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

	private static String pluginName="satellite"; 
	
	private Map<String,GiniAreaProvider> availAreasMap = new HashMap<String, GiniAreaProvider>();

	@Override
	public AreaSource getAreaSource() {
		return sourceName;
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
	
	public GiniAreaProviderFactory() {		
	}

	@Override
	public void initialize( String srcName, String dataLoc, String configData ) throws VizException {
		
		sourceName = AreaSource.createImagaeBasedAreaSource( srcName );
		
		HashMap<String, RequestConstraint> reqConstraints = new HashMap<String, RequestConstraint>();
		reqConstraints.put( "pluginName", new RequestConstraint( pluginName) );

        try {            
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints( reqConstraints );
            request.addRequestField("creatingEntity");
            request.addRequestField("sectorID");           
            request.setDistinct(true);
            DbQueryResponse response = (DbQueryResponse) ThriftClient.sendRequest(request);

            for( Map<String, Object> result : response.getResults() )  {
            	String satAreaName = result.get("creatingEntity")+File.separator+result.get("sectorID");
            	if( !availAreasMap.containsKey( satAreaName ) ) {
                	availAreasMap.put( satAreaName, new GiniAreaProvider( satAreaName ) ); // get geom later when requested.            		
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
			availAreasMap.put( areaName, new GiniAreaProvider( areaName ) );
		}
		
		return availAreasMap.get( areaName );
	}	
//	@Override
//	public void setInitializationData(IConfigurationElement config,
//			String propertyName, Object data) throws CoreException {
//		System.out.println("setInitializationData called with propertyName "+
//				propertyName+ " = "+data.toString() );
//	}

	// TODO : recieve data uris, get area name and check if new and if 
	// so query and update the sat coverage.
	
}