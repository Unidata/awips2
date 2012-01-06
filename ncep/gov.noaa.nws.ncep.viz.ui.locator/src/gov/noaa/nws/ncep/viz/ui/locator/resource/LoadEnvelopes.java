package gov.noaa.nws.ncep.viz.ui.locator.resource;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Envelope;

public class LoadEnvelopes extends Thread { //implements Runnable {
	private static List<Object[]> allBounds = null;
	private LocatorTableReader locTbl = null;
    private List<Locator> locatorList = null;
    
    private static HashMap<Envelope,List<Object[]>> Envelopes = null;
    private static HashMap<String,HashMap<Envelope,List<Object[]>>> LocEnvelopes = null;
	
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
  
    public LoadEnvelopes(){
    	start();
    	yield();
    }
    private static final List<Envelope> getEnv(){
    	List<Envelope> envs = new ArrayList<Envelope>();
    	envs.add(envs1); envs.add(envs2); envs.add(envs3); envs.add(envs4);
    	envs.add(envs5); envs.add(envs6); envs.add(envs7);envs.add(envs8); 
    	envs.add(envs9); envs.add(envs10); envs.add(envs11); envs.add(envs12);
    	envs.add(envs13);//envs.add(envs14); envs.add(envs15); envs.add(envs16);    	
    	return envs;
    }
    
    private static final List<Envelope> envs = getEnv();
    
    public static HashMap<Envelope,List<Object[]>> getbndEnv(String locNam){
    	if ( LocEnvelopes.containsKey(locNam) ){
    		return LocEnvelopes.get(locNam);
    	}
    	else 
    		return null;
    }	
    
    
    @Override
    public void run() {
    	try {
    		locTbl = new LocatorTableReader(
    	  			LocalizationManager.getInstance().getFilename("locatorTable"));
    		
    		locatorList = locTbl.getLocatorTable();
    		LocEnvelopes = new HashMap<String,HashMap<Envelope, List<Object[]>>>();
    		for(Locator itm : locatorList){
    			String locNam = itm.getLocatorName();
				if ( !(locNam.equals("SFSTATION")) && !(locNam.equals("LATLON"))&&itm.getShapefileName()!=null&&(! itm.getShapefileName().contains("stns")) 
						&& !(locNam.equals("CITIES")) && !(locNam.equals("CITIES_TEST"))) {
					String filNam = itm.getShapefileName();
					String attNam = itm.getAttributeName();
					String attId = itm.getAttributeID();					
					Envelopes = new HashMap<Envelope,List<Object[]>>();
	    			for (Envelope env : envs) {
    					Envelopes.put(env, getBounds(env, filNam, attNam, attId));
    				}
    				if (Envelopes != null) LocEnvelopes.put(locNam, Envelopes); 
    			}
    		}    
    	}catch (JAXBException e1) {
    			// TODO Auto-generated catch block
    			e1.printStackTrace();
    	} catch ( Exception e) {
    		System.out.println("Error loading Bounds from DB: "+ e.getMessage());
    	}
    	
    }
    
    
    private static List<Object[]> getBounds(Envelope env, String filename, String labelname, String labelid) {
    	    
    	StringBuilder query = new StringBuilder();
    	query.append("Select AsBinary(the_geom), "+labelname + " ," +
			labelid + " FROM " + filename);
    	String geoConstraint = String.format(
			" WHERE the_geom && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326);",
	        env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY());
    	query.append(geoConstraint);
    	
    	//System.out.println(query.toString());
			
    	//if (allBounds == null) {
    	try {
    		allBounds = NcDirectDbQuery.executeQuery( // DirectDbQuery.executeQuery(
    			query.toString(), "ncep", QueryLanguage.SQL);
	        //if (allBounds != null )System.out.println("Query successful");
	        //else System.out.println("Query wrong");
    	}catch (VizException ve ){
    		ve.printStackTrace();
    		System.out.println("db exception!");
    	}
    	return allBounds;
    }

	
}
