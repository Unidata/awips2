package gov.noaa.nws.ncep.viz.rsc.ffg;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

/*
 * This class parses NMAP2's zones.stn file which contains the following data in fixed size fields:
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  04/01/2009             Greg Hull    Initial creation.
 * 
 * </pre>
 * 
 * @author ghull 
 * @version 1.0
 * 
 */
public class CountyZoneInfo {
    private final String CZfileName;

    private final HashMap<String, ZoneEntry> czMap;

    public class ZoneEntry {
    	public ZoneEntry( String zid, String nm, double t, double n ) {
    		zoneId = zid;
    		name = nm;
    		lat = t;
    		lon = n;
    	}
    	private String zoneId; 
    	private String name;
        private  double lat, lon;
        
        public String getZoneId() {
        	return zoneId;
        }
        public String getName() {
        	return name;
        }
        public double getLat() {
        	return lat;
        }
        public double getLon() {
        	return lon;
        }
    }

    public CountyZoneInfo(String fname) {
        CZfileName = fname;
        czMap = new HashMap<String, ZoneEntry>();
    }

    public ZoneEntry getZone( String zoneID ) {
        return this.czMap.containsKey(zoneID) ? this.czMap.get(zoneID) : null;
    }

    public String getFileName() {
        return this.CZfileName;
    }

    // throw an exception if there is a problem opening the file.
    public boolean loadCounties() throws FileNotFoundException, IOException {
        BufferedReader input = new BufferedReader(new FileReader(new File(CZfileName)));
        String lineStr = null;

        while( (lineStr=input.readLine()) != null ) {
        	if( lineStr.startsWith("!") ) {
        		continue;
        	}

        	String zoneIdStr   = lineStr.substring(0,8).trim();
        	String zoneNameStr = lineStr.substring(9,15).trim();
        	String zoneDescrStr = lineStr.substring(16,48).trim();
        	String zoneStateStr = lineStr.substring(49,51).trim();
        	String zoneCntryStr = lineStr.substring(52,54).trim();
        	String zoneLatStr  = lineStr.substring(55,60).trim();
        	String zoneLonStr = lineStr.substring(61,67).trim();

        	czMap.put( zoneIdStr, 
        			new ZoneEntry( zoneIdStr, zoneNameStr, 
        					Double.parseDouble(zoneLatStr) / 100, 
        					Double.parseDouble(zoneLonStr) / 100 ) );
        }
        input.close();

        return (czMap.size() > 0 ? true : false );
    }
}
