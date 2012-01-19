package gov.noaa.nws.ncep.edex.common.stationTables;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.quadtree.Quadtree;
import org.geotools.referencing.GeodeticCalculator;

/**
 * This class reads a station table from an xml file and contains a list of stations.
 * This class also provide general station search functions given station field, and
 * field value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/09  		?    	   	B. Yin   Initial Creation
 * 06/09		134			M. Li		Add station search
 * 10/09        39/87/114   L. Lin   Make "last" as private StationField.
 * 12/09		159			B. Yin	 Add getNearestStation(...)
 *                       
 * </pre>
 * 
 * @author bingfan
 * @version 1
 */

public class StationTable implements IStationField {
	
	private final String PACKAGE = "gov.noaa.nws.ncep.edex.common.stationTables";
	
	private List<Station> stationList;
	
	private StationField last = null;
	
	private Quadtree stTree = null;
	
	private final double DIST = 1.0;
	
	/**
	 * Constructor. 
	 * @param tableFileName - full path of the xml table file
	 */
    public StationTable( String tableFileName ) {
    	
    	try{
    		stationList = readStationTable( tableFileName );
    	}
    	catch ( JAXBException exp ){
    		stationList = null;
    		exp.printStackTrace();
    	}
    	
	}

    /**
     * Reads the contents of the input station table file
     * @param xmlFilename - full path of the xml table name
     * @return - a list of stations
     * @throws JAXBException
     */
	private List<Station> readStationTable( String xmlFilename ) throws JAXBException{

	    File xmlFile = new File(xmlFilename);
	    
        JAXBContext context = JAXBContext.newInstance(
           PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        StationList stns = null;
        
		try {
			stns = (StationList)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<Station> listOfItems = stns.getStation();			
			
			/*
			 * save stations in a Quadtree for efficient spatial query
			 */
			stTree = new Quadtree();
			for ( Station st : listOfItems ) {
				Coordinate c = new Coordinate(st.getLongitude(), st.getLatitude());
				Envelope env = new Envelope(c.x-DIST, c.x+DIST, c.y-DIST, c.y+DIST);
				stTree.insert(env, st);
			}
			
			return  listOfItems;
		    
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
			
		} catch (NullPointerException e2) {
			e2.printStackTrace();		
		}	
		
		return null;
              
    }   
	
	/**
	 * Gets the list of the stations
	 * @return - the list of stations
	 */
	public List<Station> getStationList(){
		
		return stationList;
		
	}
	
	/**
	 *  Search a station given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Station
	 */
	public Station getStation(StationField sf, String key) {
		if (stationList == null || stationList.isEmpty()) return null;
		
		StationComparator comparator = new StationComparator(sf);
		if (last == null || (last != null && last != sf )) {
			Collections.sort(stationList, comparator);
			last = sf;
		}
		
		Station s = getComparedStation(sf, key);
		int index = Collections.binarySearch(stationList, s, comparator);

		if (index >= 0){
			return stationList.get(index);
		} else
			return null;
	}
	
	/**
	 *  Search station list given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Station
	 */
	public List<Station> getStations(StationField sf, String key) {
		if (stationList == null || stationList.isEmpty()) return null;
		
		StationComparator comparator = new StationComparator(sf);
		if (last == null || (last != null && last != sf )) {
			Collections.sort(stationList, comparator);
			last = sf;
		}
		
		List<Station> list = new ArrayList<Station>();
		
		Station s = getComparedStation(sf, key);
		int index;
		while ((index = Collections.binarySearch(stationList, s, comparator)) >= 0) {
			list.add(stationList.get(index));
			stationList.remove(index);
		}
		
		if (list.size() > 0) {
			for (Station st : list) {
				stationList.add(st);
			}

			last = null;
			return list;
		}
		else {
			return null;
		}
	}
	
	
	private Station getComparedStation(StationField sf, String key){
		Station station = new Station();
		switch (sf) {
		case STID:
			station.setStid((String)key);
			break;
		case STNM:
			station.setStnnum((String)key);
			break;
		case NAME:
			station.setStnname((String)key);
			break;
		case ST:
			station.setState((String)key);
			break;
		case CO:
			station.setCountry((String)key);
			break;		
		/*
		case LAT:
			station.setLatitude((Float)key);
			break;
		case LON:
			station.setLongitude((Float)key);
			break;
		case ELV:
			station.setElevation((Integer)key);
			break;	
		case PRI:
			station.setPriority((Integer)key);
			break;
		*/		
		case WFO:
			station.setWfo((String)key);
			break;	
		case LOC:
			station.setLocation((String)key);
			break;	
		}
		
		return station;
	}
	
	/**
	 * Get the nearest station from the input location
	 * @param loc
	 * @return
	 */
	public Station getNearestStation( Coordinate loc) {
		
		double min = Double.MAX_VALUE;
		Station found = null;
		GeodeticCalculator gc =  new GeodeticCalculator();
		gc.setStartingGeographicPoint(loc.x, loc.y);
		
		Envelope searchEnv = new Envelope(loc.x-DIST, loc.x+DIST, loc.y-DIST, loc.y+DIST);
		
		List<?> res = stTree.query(searchEnv);
		Iterator<?> iter = res.iterator();
		
		while ( iter.hasNext() ) {
			Station st = (Station)iter.next();
			Coordinate where = new Coordinate(st.getLongitude(), st.getLatitude());
			gc.setDestinationGeographicPoint(where.x, where.y);
			
			double dist = gc.getOrthodromicDistance();
			if ( dist < min ) {
				min = dist;
				found = st;
			}
			
		}
		
		return found;
	}
}