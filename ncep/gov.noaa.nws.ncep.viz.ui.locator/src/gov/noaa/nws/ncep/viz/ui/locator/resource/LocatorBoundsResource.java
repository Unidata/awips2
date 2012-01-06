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
 * </pre>
 * 
 * @author J. Zeng
 * 
 * 
 */
//import java.io.File;
//import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;


import java.io.IOException;
//import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

//import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

public class LocatorBoundsResource extends Thread {
	
	private String filename = null;
	
	private String locatorname = null;

    private int labelfield = 0;
    
    private Locator currentLocator;
    
    private List<Object[]> BoundsDataStore;
    
   
    
    private HashMap<Envelope,List<Object[]>> savedEnvelopes = null;
    private HashMap<Locator,HashMap<Envelope,List<Object[]>> > savedStores = null;
    
  
    
    /**
     * Construct a boundstable resource
     * 
     * @param file
     */
    public LocatorBoundsResource() {
    	savedStores = new HashMap<Locator,HashMap<Envelope,List<Object[]>>>();
    	//checkStore();
    }
    
    public LocatorBoundsResource(Locator loc) {
    	this();
    	this.currentLocator = loc;
    	
    }
    
    
    /**
     * Retrieve values from a shape file without needing to 
     * load it into the map. 
     * @throws IOException 
     * @throws RuntimeException 
     * @throws IOException 
     *      
     */
    public String getBoundsValue(Coordinate aLatLon) throws VizException, RuntimeException, IOException {
    	
    	String bndVal = null;
    	
    	//filename = currentLocator.getShapefileName();
    	//System.out.println("Bound file: "+ filename);
    	
    	
    	if ( filename!= null && filename.equals("stns.cities")) {
    		citiesPointData cities = new citiesPointData();
        	return cities.calculateNearestPoint(aLatLon, currentLocator.getDisplayOptions());    		
        } 
        if(locatorname.equals("SFSTATION")){
        	return SurfaceStationPointData.calculateNearestPoint(aLatLon);   
        	//return("STN lon = "+ aLatLon.x+ " lat = "+ aLatLon.y );
        }
    	//this.checkStore();
    	if ( savedStores.containsKey(currentLocator) ) {
    		savedEnvelopes = savedStores.get(currentLocator);
    		BoundsDataStore = null;
    		for ( Envelope env : savedEnvelopes.keySet() ) {
    			if ( env.contains(aLatLon) ) {
   	    		    BoundsDataStore = savedEnvelopes.get(env);
    				break;
    			}
    		}
    	}
    	else {
    		//System.out.println("REALLY?!?!?!?!?!?!!?!?!?!?!?!??!!?");
    		return new String("Unavailable");    		
    	}
    	
    	String attribute = null;
    	
    	if ( currentLocator != null ) {
        	
        	attribute = currentLocator.getDisplayOptions().getDisplayAttribute();
            if (attribute == null || attribute.equalsIgnoreCase(LocatorTool.STATIONDISPLAY_OPTIONS[0])) {
         		labelfield = 1;
           	} else {
           		labelfield = 2;
           	}
    	}
        GeometryFactory gf = new GeometryFactory();
	    Point llPoint = gf.createPoint(aLatLon);

	    java.util.TreeSet<Integer> ss = new java.util.TreeSet<Integer>(); 
	    String s = currentLocator.getShapefileName(), el = "bounds.elev_nam1000";  
	    
	   	WKBReader wkbReader = new WKBReader();
	    for ( Object[] bound : BoundsDataStore){
	    	byte[] wkb = (byte[]) bound[0];
			MultiPolygon boundGeo = null;
			try {
		    	boundGeo = (MultiPolygon) wkbReader.read(wkb);		    			
		    } catch (ParseException e) {
		    	e.printStackTrace();
		    }
		    if (boundGeo.contains(llPoint) ){
		    	bndVal = (String)bound[labelfield];

		    	if(s !=null && s.contains(el)){
		    		int i=0; 
		    		try{
		    			i=Integer.parseInt(bndVal);
		    		}catch(Exception e){	}
		    		ss.add(i);
		    	}else
		    		return bndVal;
		    }
	    }
	    
	    String ll = ss.size() > 0 ? ""+ss.last() : null;
	    ss.clear();
	    
    	return ll;//null;
    }

    
    public void setCurrentLocator(Locator loc) {
    	currentLocator = loc;
    	filename = currentLocator.getShapefileName();
    	locatorname = currentLocator.getLocatorName();
    	checkStore();
    }
    
    private void checkStore()  {
    	
    	if( locatorname.equals("CITIES") || locatorname.equals("SFSTATION")) {
    		return;
    	}
    	
    	//System.out.println(savedStores.toString() );
    	if ( ! savedStores.containsKey(currentLocator) ) {
    		savedEnvelopes = LoadEnvelopes.getbndEnv(locatorname);
        	if ( savedEnvelopes != null) {
    		    savedStores.put(currentLocator,savedEnvelopes);
        	}
        	//System.out.println("HOW MANY? = "+savedStores.size());        	
    	}
    	
    }
    
    
}
