/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.monitor.config;

import java.util.ArrayList;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.ObsHistType;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.monitor.xml.MonAreaConfigXML;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.SiteMap;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            mpduff     Initial creation
 * Apr 29, 2011 DR#8986   zhao       Read in Counties instead of Forecast Zones
 * 
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public abstract class MonitorConfigurationManager {
	
	public static final String COUNTY_TABLE = "mapdata.county"; 
	
	public static final String FORECAST_ZONE_TABLE = "mapdata.zone"; 
	
    public static final String MARINE_ZONE_TABLE = "mapdata.marinezones";

    /**
     * Monitoring Area Configuration XML object.
     */
    protected MonAreaConfigXML configXml;

    /**
     * List of newly added zones.
     */
    protected ArrayList<String> addedZones = new ArrayList<String>();
    
    /**
     * List of newly added stations.
     */
    protected ArrayList<String> addedStations = new ArrayList<String>();
    

    public abstract void readConfigXml(String currentSite);
    
    /**
     * Read the XML configuration data for the current XML file name.
     */
    protected void readConfigXml(String currentSite, String filename) {
        boolean fileExists = true;
        try {
            // configXml = null;
            IPathManager pm = PathManagerFactory.getPathManager();

            String path = pm.getFile(
                    pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.SITE), filename)
                    .getAbsolutePath();

            System.out.println("Read path = " + path);

            MonAreaConfigXML configXmltmp = (MonAreaConfigXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path.toString());
            configXml = configXmltmp;
        } catch (Exception e) {
            // e.printStackTrace();
            System.err.println("No configuration file found");
            fileExists = false;
        }

        try {
            // Check for a config file, if one does not exist use defaults
            /**
        	 * Note: Read in "county" for CONUS site, "forecast zone" for OCONUS site
        	 * [DR#9905]
        	 */
            if (!fileExists) {
            	ArrayList<String> zones; 
            	if ( SiteMap.getInstance().getSite4LetterId(currentSite).charAt(0) == 'K' ) { // CONUS site
            		zones = MonitorAreaUtils.getUniqueCounties(currentSite); 
            	} else { // OCONUS site
            		zones = MonitorAreaUtils.getForecastZones(currentSite);
                }
            	ArrayList<String> marineZones = MonitorAreaUtils.getMarineZones(currentSite);

                if (zones.size() > 0) {
                    for (String zone: zones) {
                    	AreaIdXML zoneXml = new AreaIdXML();
                    	zoneXml.setAreaId(zone);
                    	zoneXml.setType(ZoneType.REGULAR);
                    	ArrayList<StationIdXML> stations = MonitorAreaUtils.getZoneReportingStationXMLs(zone);
                    	if ( stations.size() > 0 ) {
                    		for ( StationIdXML station : stations ) {
                    			zoneXml.addStationIdXml(station);
                    		}
                    	}
                        configXml.addAreaId(zoneXml);            
                    }
                }
                
                // add marine zones if any exist
                if (marineZones.size() > 0) {
                    for (String zone : marineZones) {
                    	AreaIdXML zoneXml = new AreaIdXML();
                    	zoneXml.setAreaId(zone);
                    	zoneXml.setType(ZoneType.MARITIME);
                    	ArrayList<StationIdXML> stations = MonitorAreaUtils.getZoneReportingStationXMLs(zone);
                    	if ( stations.size() > 0 ) {
                    		for ( StationIdXML station : stations ) {
                    			zoneXml.addStationIdXml(station);
                    		}
                    	}
                        configXml.addAreaId(zoneXml);            
                    }
                }
                
                saveConfigXml(filename);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Save the XML configuration data to the current XML file name.
     */
    protected void saveConfigXml(String filename) {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                filename);

        if (newXmlFile.getFile().getParentFile().exists() == false) {
            System.out.println("Creating new directory");

            if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("Could not create new directory...");
            }
        }

        try {
            System.out.println("Saving -- "
                    + newXmlFile.getFile().getAbsolutePath());
            SerializationUtil.jaxbMarshalToXmlFile(configXml, newXmlFile
                    .getFile().getAbsolutePath());
            newXmlFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Add a new Area to the configuration. This method only adds the area, the
     * other info will need to be added to the area via the other methods in
     * this class.
     * 
     * @param areaId
     *            The area Id (name)
     */
    public void addArea(String areaId, ZoneType type) {
        ArrayList<AreaIdXML> areaXmlList = configXml.getAreaIds();
        boolean areaExists = false;
        for (AreaIdXML area: areaXmlList) {
            if (area.getAreaId().equals(areaId)) {
                area.setType(type);
                areaExists = true;
                break;
            }
        }
        
        if (areaExists == false) {
            AreaIdXML area = new AreaIdXML();
            area.setAreaId(areaId);
            area.setType(type);
            configXml.addAreaId(area);            
        }
    }
    
    /**
     * Add a new Area to the configuration. This method only adds the area, the
     * other info will need to be added to the area via the other methods in
     * this class.
     * 
     * @param areaId
     *      The area id
     * @param lat
     *      The area latitude
     * @param lon
     *      The area longitude
     * @param type
     *      The area type
     * @param existingArea
     *      Does the area already exist
     */
    public void addArea(String areaId, double lat, double lon, ZoneType type, boolean existingArea) {
        ArrayList<AreaIdXML> areaXmlList = configXml.getAreaIds();
        boolean areaExists = false;
        for (AreaIdXML area: areaXmlList) {
            if (area.getAreaId().equals(areaId)) {
                area.setType(type);
                area.setCLat(lat);
                area.setCLon(lon);
                areaExists = true;
                break;
            }
        }
        
        if (areaExists == false) {
            AreaIdXML area = new AreaIdXML();
            area.setAreaId(areaId);
            area.setType(type);
            area.setCLat(lat);
            area.setCLon(lon);
            configXml.addAreaId(area);            
        }
        
        if ((existingArea == false) && !addedZones.contains(areaId)) {
            addedZones.add(areaId);
        }
    }

    /**
     * Add a station to the area.
     * 
     * @param areaId
     *            The area id to add the station to
     * @param stationId
     *            The station id
     * @param type
     *            The station type
     */
    public void addStation(String areaId, String stationId, String type, boolean existingStation) {
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        if (stationId.contains("#")) {
            stationId = stationId.substring(0, stationId.indexOf("#"));
        }

        for (AreaIdXML area: areaList) {
            if (area.getAreaId().equals(areaId)) {
                StationIdXML stationXml = new StationIdXML();
                stationXml.setName(stationId);
                stationXml.setType(type);
                area.addStationIdXml(stationXml);
                addedStations.add(stationId);                
            }
        }
    }

    /**
     * Get the areas of a particular type.
     * 
     * @param type
     *            ZoneType of the area
     * @return List of areas of the specified type
     */
    public ArrayList<String> getAreasByType(ZoneType type) {
        ArrayList<String> results = new ArrayList<String>();
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();

        for (AreaIdXML area : areaList) {
            if (area.getType().equals(type)) {
                results.add(area.getAreaId());
            }
        }

        results.trimToSize();

        return results;
    }

    /**
     * Get stations associated with an area.
     * 
     * @param areaId
     *      AreaId of associated stations
     * @return
     *      List of stations for area
     */
    public ArrayList<String> getAreaStationsWithType(String areaId) {
        ArrayList<String> results = new ArrayList<String>();
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area: areaList) {
            if (area.getAreaId().equals(areaId)) {
                ArrayList<StationIdXML> stationList = area.getStationIds();

                for (StationIdXML station : stationList) {
                    results.add(station.getName() + "#" + station.getType());
                }
               
            }
        }

        return results;
    }
    
    /**
     * Get stations associated with an area.
     * 
     * @param areaId
     *      AreaId of associated stations
     * @return
     *      List of stations for area
     */
    public ArrayList<String> getAreaStations(String areaId) {
        ArrayList<String> results = new ArrayList<String>();
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area: areaList) {
            if (area.getAreaId().equals(areaId)) {
                ArrayList<StationIdXML> stationList = area.getStationIds();

                for (StationIdXML station : stationList) {
                    results.add(station.getName());
                }
               
            }
        }

        return results;
    }
    
    /**
     * Get an area of a station.
     * 
     * @param stationId
     *      The station to get the area
     * @return
     *      List of areas
     */
    public ArrayList<String> getAreaByStationId(String stationId) {
        ArrayList<String> results = new ArrayList<String>();
        
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        
        for (AreaIdXML area: areaList) {
            ArrayList<StationIdXML> stationList = area.getStationIds();
            for (StationIdXML station: stationList) {
                if (station.getName().equals(stationId)) {
                    results.add(area.getAreaId());
                }
            }
        }        
        
        return results;
    }
    
    /**
     * Get all the stations associated with the areas.
     * @return
     */
    public ArrayList<String> getStations() {
        ArrayList<AreaIdXML> areaXml = configXml.getAreaIds();
        ArrayList<String> stations = new ArrayList<String>();
        
        for (AreaIdXML area: areaXml) {
            ArrayList<StationIdXML> stationList = area.getStationIds();
            for (StationIdXML station: stationList) {
                stations.add(station.getName() + "#" + station.getType() + "#" + area.getAreaId());
            }
        }
        
        return stations;
    }

    /**
     * Get a list of all the areas.
     * 
     * @return ArrayList<String> of area ids
     */
    public ArrayList<String> getAreaList() {
        ArrayList<AreaIdXML> areaXmlList = configXml.getAreaIds();
        ArrayList<String> areaList = new ArrayList<String>();

        for (AreaIdXML area : areaXmlList) {
            areaList.add(area.getAreaId());
        }

        areaList.trimToSize();

        return areaList;
    }

    /**
     * Remove a station from the area.
     * 
     * @param area
     *            Area to remove the station from
     * @param station
     *            Station to remove from the area
     */
    public void removeStation(String area, String station) {
        station = station.substring(0, station.indexOf("#"));
        
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        
        for (AreaIdXML areaXml: areaList) {
            if (areaXml.getAreaId().equals(area)) {
                ArrayList<StationIdXML> stationList = areaXml.getStationIds();

                for (int i = 0; i < stationList.size(); i++) {
                    if (stationList.get(i).getName().equals(station)) {
                        stationList.remove(i);
                    }
                }                
            }
        }
    }
    
    /**
     * Remove a station from the monitoring area.
     * 
     * @param station
     *      The station to remove
     */
    public void removeStation(String station) {
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        
        for (AreaIdXML areaXml: areaList) {
            ArrayList<StationIdXML> stationList = areaXml.getStationIds();
            for (int i = 0; i < stationList.size(); i++) {
                StationIdXML stationXml = stationList.get(i);
                if (stationXml.getName().equals(station)) {
                    stationList.remove(i);
                    i--;
                }
            }
        }
    }
    
    /**
     * Get an AreaIdXML object.
     * 
     * @param area
     *      The area to get
     * @return
     *      The AreaIdXML object
     */
    public AreaIdXML getAreaXml(String area) {
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();

        for (AreaIdXML areaXml: areaList) {
            if (areaXml.equals(area)) {
                return areaXml;
            }
        }
        
        return null;
    }
    
    /**
     * Remove an area from the monitoring area.
     * 
     * @param area
     *      The area to remove
     */
    public void removeArea(String area) {
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();

        for (int i = 0; i < areaList.size(); i++) {
            if (areaList.get(i).getAreaId().equals(area)) {                 
                areaList.remove(i);
                break;
            }
        }
        
        for (int i = 0; i < addedZones.size(); i++) {
            if (addedZones.get(i).equals(area)) {
                addedZones.remove(i);
                break;
            }
        }
    }
    
    /**
     * Remove an added area.
     * 
     * @param area
     *      The area to remove
     */
    public void removeAddedArea(String area) {
        for (int i = 0; i < addedZones.size(); i++) {
            if (addedZones.get(i).equals(area)) {
                addedZones.remove(i);
                break;
            }
        }
    }
    
    /**
     * @return the timeWindow
     */
    public int getTimeWindow() {
        return configXml.getTimeWindow();
    }

    /**
     * @param timeWindow
     *            the timeWindow to set
     */
    public void setTimeWindow(int timeWindow) {
        configXml.setTimeWindow(timeWindow);
    }

    /**
     * @return the configXml
     */
    public MonAreaConfigXML getConfigXml() {
        return configXml;
    }

    /**
     * @return the addedZones
     */
    public ArrayList<String> getAddedZones() {
        return addedZones;
    }

    /**
     * @param addedZones the addedZones to set
     */
    public void setAddedZones(ArrayList<String> addedZones) {
        this.addedZones = addedZones;
    }

    /**
     * @return the addedStations
     */
    public ArrayList<String> getAddedStations() {
        return addedStations;
    }

    /**
     * @param addedStations the addedStations to set
     */
    public void setAddedStations(ArrayList<String> addedStations) {
        this.addedStations = addedStations;
    }

	public ObsHistType getStationType(String theZone, String theStation) {
        ObsHistType result = null; 
        ArrayList<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area: areaList) {
            if (area.getAreaId().equals(theZone)) {
                ArrayList<StationIdXML> stationList = area.getStationIds();

                for (StationIdXML station : stationList) {
                    if ( station.getName().equals(theStation) ) {
                    	String typeString = station.getType();
                    	result = typeString.equals("METAR")?ObsHistType.METAR:ObsHistType.Maritime;
                    }
                }
            }
        }

		return result;
	}
}
