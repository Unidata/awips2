package gov.noaa.nws.ncep.viz.ui.locator.resource;

/**
 * Bounds resource for Locator.
 * 
 * 
 * <pre>
 *     
 *      SOFTWARE HISTORY
 *     
 *      Date            Ticket#     Engineer    Description
 *      ------------	----------	-----------	--------------------------
 *      02/2010			222          J.Zeng      Initial  created
 *      11/08/10        229        Chin Chen   Add SFSTATION locator for NSHARP 
 *      
 * </pre>
 * 
 * @author J. Zeng
 * 
 * 
 */
//import java.io.File;
//import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;


import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.io.IOException;
//import java.util.ArrayList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

//import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

public class BoundsDataSource {
		
    private LocatorDataSource dataSource;
        
    private HashMap<Envelope,List<Object[]>> boundsDataByEnvelope = null;
    
	private static final Envelope envs1 = new Envelope(-127.0, -91.0,   40.0, 52.0); //nw
    private static final Envelope envs2 = new Envelope(-127.0, -91.0,   24.0, 40.0); //sw
    private static final Envelope envs3 = new Envelope(-91.0, -50.0,    40.0, 50.0); //ne
    private static final Envelope envs4 = new Envelope(-91.0, -50.0,    18.0, 40.0); //se
    private static final Envelope envs5 = new Envelope(-180.0, -122.0, 14.0, 73.0); //pac
    
    private static final Envelope envs6 = new Envelope(-180.0, -90.0,	0.0, 90.0);
    private static final Envelope envs7 = new Envelope(-180.0, -90.0,	0.0, -90.0);
    private static final Envelope envs8 = new Envelope(-90.0, 0.0, 	0.0, 90.0);
    private static final Envelope envs9 = new Envelope(-90.0, 0.0, 	0.0, -90.0);
    private static final Envelope envs10 = new Envelope(0.0, 90.0,		0.0, 90.0);
    private static final Envelope envs11 = new Envelope(0.0, 90.0,		0.0, -90.0);
    private static final Envelope envs12 = new Envelope(90.0, 180.0,	0.0, 90.0);
    private static final Envelope envs13 = new Envelope(90.0, 180.0,	0.0, -90.0);
	private List<Envelope> allEnvelopes = new ArrayList<Envelope>();

	// TODO : this was changed so that only 1 field is stored. This can be 
	// changed to again query and store 2 (or more) fields but the data sources
	// will need to be preprocessed to find common dbTables and combine fields.
	// 
    private int labelfield = 1;
    
    public BoundsDataSource(LocatorDataSource ds) {
    	this.dataSource = ds;
    	boundsDataByEnvelope = new HashMap<Envelope,List<Object[]>>();
    }
    
	public void loadData( ) throws VizException {

    	allEnvelopes.add(envs1); 
    	allEnvelopes.add(envs2); 
    	allEnvelopes.add(envs3); 
    	allEnvelopes.add(envs4);
    	allEnvelopes.add(envs5); 
    	allEnvelopes.add(envs6); 
    	allEnvelopes.add(envs7);
    	allEnvelopes.add(envs8); 
    	allEnvelopes.add(envs9); 
    	allEnvelopes.add(envs10); 
    	allEnvelopes.add(envs11); 
    	allEnvelopes.add(envs12);
    	allEnvelopes.add(envs13);
    	//envs.add(envs14); envs.add(envs15); envs.add(envs16);    	
    	
    	for( Envelope env : allEnvelopes ) {

    		StringBuilder query = new StringBuilder();
    		query.append("Select AsBinary(the_geom), "+ 
    				dataSource.getDbFieldName()+ " FROM " + dataSource.getDbTableName() );
    		//	query.append("Select AsBinary(the_geom), "+labelname + " ," +
    		//			labelid + " FROM " + filename);
    		String geoConstraint = String.format(
    				" WHERE the_geom && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326);",
    				env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY());
    		query.append(geoConstraint);

    		List<Object[]> boundsData = null;

    		try {
    			boundsData = NcDirectDbQuery.executeQuery( // DirectDbQuery.executeQuery(
    					query.toString(), dataSource.getDbName(), QueryLanguage.SQL);

    			boundsDataByEnvelope.put( env, boundsData );

    			//if (allBounds != null )System.out.println("Query successful");
    			//else System.out.println("Query wrong");
    		}catch (VizException ve ){
    			ve.printStackTrace();
    			System.out.println("db exception!");
    			throw ve;
    		}
    	}
    }
    
    public String getBoundsValue( Coordinate aLatLon, LocatorDisplayAttributes dispAttrs ) 
    			throws VizException, RuntimeException, IOException {
    	
    	String bndVal = null;
    	
    	List<Object[]> boundsData = null;

    	for ( Envelope env : boundsDataByEnvelope.keySet() ) {
    		if ( env.contains(aLatLon) ) {
    			boundsData = boundsDataByEnvelope.get(env);
    			break;
    		}
    	}

    	GeometryFactory gf = new GeometryFactory();
	    Point llPoint = gf.createPoint(aLatLon);

	    java.util.TreeSet<Integer> ss = new java.util.TreeSet<Integer>(); 
	    String dbTbl = dataSource.getDbTableName();
	    String el = "bounds.elev_nam1000";  
	    
	   	WKBReader wkbReader = new WKBReader();
	   	
	    for ( Object[] bound : boundsData ){
	    	byte[] wkb = (byte[]) bound[0];
			MultiPolygon boundGeo = null;
			try {
		    	boundGeo = (MultiPolygon) wkbReader.read(wkb);		    			
		    } catch (ParseException e) {
		    	e.printStackTrace();
		    }
		    
		    if (boundGeo.contains(llPoint) ){
		    	bndVal = (String)bound[labelfield];

		    	if( dbTbl !=null && dbTbl.contains(el)){
		    		int i=0; 
		    		try{
		    			i=Integer.parseInt(bndVal);
		    		} catch(Exception e){	}
		    		ss.add(i);
		    	}else
		    		return bndVal;
		    }
	    }
	    
	    String ll = ss.size() > 0 ? ""+ss.last() : null;
	    ss.clear();
	    
    	return ll;//null;
    }    
}
