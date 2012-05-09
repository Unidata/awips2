/*
 * gov.noaa.nws.ncep.ui.pgen.maps
 * 
 * 23 February 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.ui.pgen.maps;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Class to hold US state information
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class USState {
	
	private static List<USState> allStates;
	private static volatile boolean stateLoaded = false; 
	
	private String stateAbrv; 	//abbreviation
	private String name;		//full name
	private String fips;
	private Coordinate centriod;
	private Geometry shape;
	
	//constructor
	public USState(){
		
	}
	
	//constructor
	public USState( String stateAbrv,				
				   String name,				
				   String fips,
				   Coordinate centroid,
				   Geometry shape ){
				   
		this.setFips(fips);
		this.setName(name);
		this.setStateAbrv(stateAbrv);
		this.setCentriod(centroid);
		this.setShape(shape);
	}

	/**
	 * Get all counties and marine zones from the database
	 * @return
	 */
	public static List<USState> getAllStates(){
		if ( !stateLoaded ){
			loadStateTable();
		}
		
		return allStates;
	}
	
	public static synchronized List<USState> loadStateTable(){
		
		if ( !stateLoaded ) {
			
			allStates = new ArrayList<USState>();

			List<Object[]> states;

			String queryStates = "Select AsBinary(the_geom),state,name,fips,lat, lon FROM mapdata.states;";

			try {
				states = NcDirectDbQuery.executeQuery(
						queryStates, "maps", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				int tt = 0;
				for ( Object[] st : states ){

						//read shape
						byte[] wkb = (byte[]) st[0];
						Geometry stateGeo = null;
						try {
							stateGeo =  wkbReader.read(wkb);
						} catch (ParseException e) {
							e.printStackTrace();
						}

						String stateAbb = (String)st[1];
						String fullName = (String)st[2];
						String fips = (String)st[3];

						Coordinate loc = new Coordinate(0,0); 
						try {
							loc.x = ((Number)st[5]).doubleValue(); 
							loc.y =	((Number)st[4]).doubleValue();
						}
						catch ( Exception e ){
							// center location missing in database 
						}

							//cntyGeo = removeSmallShells(cntyGeo, 0.001);


							//test for invalid county shapes
			//				if ( !stateGeo.isValid() ){
			//					IsValidOp vld = new IsValidOp(stateGeo);
			//					TopologyValidationError err = vld.getValidationError();
			//					ii++;
			//					System.out.println("invalid county geo: " + err.getErrorType() + err.getCoordinate() + err.getMessage() + 
			//							" " + cntyName + " " + cntyFips + " " + ii );
								
		//						if ( err.getErrorType() == 7 ) cntyGeo = fixNestedShells(cntyGeo);
								//if ( err.getErrorType() == 5 ) cntyGeo = removeSmallShells(cntyGeo, 1e-3);
		//						if ( err.getErrorType() == 3 ) cntyGeo = fixNestedHoles(cntyGeo);
		//						if ( err.getErrorType() == 2 ) cntyGeo = fixHoleOutOfShell(cntyGeo);

			//				}


		//					County existingCnty = County.findCounty( cntyFips );
							//				if ( existingCnty != null ){
							//					System.out.println( "Existing: " + existingCnty.getName() + " " + existingCnty.getState() + " " + existingCnty.getFips());
							//					System.out.println( "New:      " + cntyName + " " + cntySt + " " + cntyFips);
							//				}
							//both fips and name are same 
					
								USState state = new USState(stateAbb, fullName, fips, loc, stateGeo);
								allStates.add(state);
								tt++;
							
						}	
				

				
			}
			catch (Exception e ){
				System.out.println("db exception reading state tables!");	
				e.printStackTrace();
			}
			stateLoaded = true;
			
		}
		return allStates;
	}

	public void setStateAbrv(String state) {
		this.stateAbrv = state;
	}

	public String getStateAbrv() {
		return stateAbrv;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setFips(String fips) {
		this.fips = fips;
	}

	public String getFips() {
		return fips;
	}

	public void setCentriod(Coordinate centriod) {
		this.centriod = centriod;
	}

	public Coordinate getCentriod() {
		return centriod;
	}

	public void setShape(Geometry shape) {
		this.shape = shape;
	}

	public Geometry getShape() {
		return shape;
	}
	
	public boolean intersectGeometry(Geometry geo){
		if ( shape != null ){
			return shape.intersects(geo);
		}
		else return false;
	}
	
	public static ArrayList<USState> statesInGeometry(Geometry geo){
		loadStateTable();
		ArrayList<USState> ret= new ArrayList<USState>();
		for ( USState st : allStates){
			if ( st.intersectGeometry(geo)){
				ret.add(st);
			}
		}
		return ret;
	}
	
	public static boolean isLoaded(){
		return stateLoaded;
	}
}
