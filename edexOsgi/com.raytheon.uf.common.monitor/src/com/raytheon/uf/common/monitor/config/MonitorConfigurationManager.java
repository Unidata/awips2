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
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.ObsHistType;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;
import com.raytheon.uf.common.monitor.xml.MonAreaConfigXML;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Monitor configuration manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            mpduff     Initial creation
 * Apr 29, 2011 DR#8986   zhao       Read in Counties instead of Forecast Zones
 * Feb 21 2012  14413     zhao       add code handling "adjacent areas"
 * Nov 20 2012  1297      skorolev   Cleaned code
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class MonitorConfigurationManager {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MonitorConfigurationManager.class);

    /**
     * Maps county table in the PostgreSQL database.
     */
    public static final String COUNTY_TABLE = "mapdata.county";

    /**
     * Maps forecast zones table in the PostgreSQL database.
     */
    public static final String FORECAST_ZONE_TABLE = "mapdata.zone";

    /**
     * Maps marine zones table in the PostgreSQL database.
     */
    public static final String MARINE_ZONE_TABLE = "mapdata.marinezones";

    /**
     * Monitoring Area Configuration XML object.
     */
    protected MonAreaConfigXML configXml;

    /**
     * Adjacent Area Configuration XML object.
     */
    protected MonAreaConfigXML adjAreaConfigXml;

    /**
     * List of newly added zones.
     */
    protected List<String> addedZones = new ArrayList<String>();

    /**
     * List of newly added stations.
     */
    protected List<String> addedStations = new ArrayList<String>();

    /**
     * Station data type in the XML configuration file.
     */
    private String xmlDataType = StationIdXML.METAR;

    /**
     * Reads area configuration file.
     * 
     * @param currentSite
     */
    public abstract void readConfigXml(String currentSite);

    /**
     * Reads the XML configuration data for the current XML file name. filename:
     * monitor area config file name adjAreaFileName: adjacent areas config file
     * name
     * 
     * @param currentSite
     * @param filename
     * @param adjAreaFilename
     */
    protected void readConfigXml(String currentSite, String filename,
            String adjAreaFilename) {
        boolean monitorAreaFileExists = true;
        boolean adjacentAreaFileExists = true;
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            String monitorAreaFilePath = pm.getFile(
                    pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.SITE), filename)
                    .getAbsolutePath();
            MonAreaConfigXML configXmltmp = (MonAreaConfigXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(monitorAreaFilePath.toString());
            configXml = configXmltmp;
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "No mopnitor area configuration file found", e);
            monitorAreaFileExists = false;
        }

        try {
            IPathManager pm = PathManagerFactory.getPathManager();

            String adjacentAreaFilePath = pm.getFile(
                    pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.SITE), adjAreaFilename)
                    .getAbsolutePath();
            MonAreaConfigXML configXmltmp = (MonAreaConfigXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(adjacentAreaFilePath.toString());
            adjAreaConfigXml = configXmltmp;
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "No adjacent area configuration file found", e);
            adjacentAreaFileExists = false;
        }

        try {
            // Check for a monitor area config file, if one does not exist,
            // create and use defaults
            /**
             * Note: Read in "county" for CONUS site, "forecast zone" for OCONUS
             * site [DR#9905]
             */
            if (!monitorAreaFileExists) {
                List<String> zones;
                if (SiteMap.getInstance().getSite4LetterId(currentSite)
                        .charAt(0) == 'K') { // CONUS site
                    zones = MonitorAreaUtils.getUniqueCounties(currentSite);
                } else { // OCONUS site
                    zones = MonitorAreaUtils.getForecastZones(currentSite);
                }
                List<String> marineZones = MonitorAreaUtils
                        .getMarineZones(currentSite);
                if (zones.isEmpty()) {
                    for (String zone : zones) {
                        AreaIdXML zoneXml = new AreaIdXML();
                        zoneXml.setAreaId(zone);
                        zoneXml.setType(ZoneType.REGULAR);
                        List<StationIdXML> stations = MonitorAreaUtils
                                .getZoneReportingStationXMLs(zone);
                        if (stations.isEmpty()) {
                            for (StationIdXML station : stations) {
                                zoneXml.addStationIdXml(station);
                            }
                        }
                        configXml.addAreaId(zoneXml);
                    }
                }
                // add marine zones if any exist
                if (marineZones.isEmpty()) {
                    for (String zone : marineZones) {
                        AreaIdXML zoneXml = new AreaIdXML();
                        zoneXml.setAreaId(zone);
                        zoneXml.setType(ZoneType.MARITIME);
                        List<StationIdXML> stations = MonitorAreaUtils
                                .getZoneReportingStationXMLs(zone);
                        if (stations.isEmpty()) {
                            for (StationIdXML station : stations) {
                                zoneXml.addStationIdXml(station);
                            }
                        }
                        configXml.addAreaId(zoneXml);
                    }
                }
                saveConfigXml(filename);
            }
            // Check for an adjacent area config file, if one does not exist,
            // create and use defaults
            if (!adjacentAreaFileExists) {
                AdjacentWfoMgr adjMgr = new AdjacentWfoMgr(currentSite);
                List<String> zones = adjMgr.getAdjZones();
                if (zones.isEmpty()) {
                    for (String zone : zones) {
                        AreaIdXML zoneXml = new AreaIdXML();
                        zoneXml.setAreaId(zone);
                        zoneXml.setType(ZoneType.REGULAR);
                        List<StationIdXML> stations = MonitorAreaUtils
                                .getZoneReportingStationXMLs(zone);
                        if (stations.isEmpty()) {
                            for (StationIdXML station : stations) {
                                zoneXml.addStationIdXml(station);
                            }
                        }
                        adjAreaConfigXml.addAreaId(zoneXml);
                    }
                }
                saveAdjacentAreaConfigXml(adjAreaFilename);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Save the monitor area XML configuration data to the current XML file
     * name.
     * 
     * @param filename
     */
    protected void saveConfigXml(String filename) {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc, filename);
        if (newXmlFile.getFile().getParentFile().exists() == false) {
            newXmlFile.getFile().getParentFile().mkdirs();
        }
        try {
            SerializationUtil.jaxbMarshalToXmlFile(configXml, newXmlFile
                    .getFile().getAbsolutePath());
            newXmlFile.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Save the adjacent area XML configuration data to the current XML file
     * name.
     * 
     * @param filename
     */
    protected void saveAdjacentAreaConfigXml(String filename) {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc, filename);
        if (newXmlFile.getFile().getParentFile().exists() == false) {
            newXmlFile.getFile().getParentFile().mkdirs();
        }
        try {
            SerializationUtil.jaxbMarshalToXmlFile(adjAreaConfigXml, newXmlFile
                    .getFile().getAbsolutePath());
            newXmlFile.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Add a new Area to the configuration. This method only adds the area, the
     * other info will need to be added to the area via the other methods in
     * this class.
     * 
     * @param areaId
     * @param type
     */
    public void addArea(String areaId, ZoneType type) {
        List<AreaIdXML> areaXmlList = configXml.getAreaIds();
        boolean areaExists = false;
        for (AreaIdXML area : areaXmlList) {
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
     *            The area id
     * @param lat
     *            The area latitude
     * @param lon
     *            The area longitude
     * @param type
     *            The area type
     * @param existingArea
     *            Does the area already exist
     */
    public void addArea(String areaId, double lat, double lon, ZoneType type,
            boolean existingArea) {
        List<AreaIdXML> areaXmlList = configXml.getAreaIds();
        boolean areaExists = false;
        for (AreaIdXML area : areaXmlList) {
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
            if (lat < -90.0 || lat > 90.0) {
                area.setCLat(lat);
            }
            if (lon < -180.0 || lon > 180.0) {
                area.setCLon(lon);
            }
            configXml.addAreaId(area);
            if (!addedZones.contains(areaId)) {
                addedZones.add(areaId);
            }
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
     * @param existingStation
     *            Does the station already exist
     */
    public void addStation(String areaId, String stationId, String type,
            boolean existingStation) {
        List<AreaIdXML> areaList = configXml.getAreaIds();
        if (stationId.contains("#")) {
            stationId = stationId.substring(0, stationId.indexOf("#"));
        }
        for (AreaIdXML area : areaList) {
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
    public List<String> getAreasByType(ZoneType type) {
        List<String> results = new ArrayList<String>();
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area : areaList) {
            if (area.getType().equals(type)) {
                results.add(area.getAreaId());
            }
        }
        return results;
    }

    /**
     * Get stations associated with an area.
     * 
     * @param areaId
     *            AreaId of associated stations
     * @return List of stations for area
     */
    public List<String> getAreaStationsWithType(String areaId) {
        List<String> results = new ArrayList<String>();
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area : areaList) {
            if (area.getAreaId().equals(areaId)) {
                List<StationIdXML> stationList = area.getStationIds();
                for (StationIdXML station : stationList) {
                    results.add(station.getName() + "#" + station.getType());
                }
            }
        }
        return results;
    }

    /**
     * Get stations associated with an adjacent area.
     * 
     * @param areaId
     *            AreaId of associated stations
     * @return List of stations for area
     */
    public List<String> getAdjacentAreaStationsWithType(String areaId) {
        List<String> results = new ArrayList<String>();
        List<AreaIdXML> areaList = adjAreaConfigXml.getAreaIds();
        for (AreaIdXML area : areaList) {
            if (area.getAreaId().equals(areaId)) {
                List<StationIdXML> stationList = area.getStationIds();
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
     *            AreaId of associated stations
     * @return List of stations for area
     */
    public List<String> getAreaStations(String areaId) {
        List<String> results = new ArrayList<String>();
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area : areaList) {
            if (area.getAreaId().equals(areaId)) {
                List<StationIdXML> stationList = area.getStationIds();
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
     *            The station to get the area
     * @return List of areas
     */
    public List<String> getAreaByStationId(String stationId) {
        List<String> results = new ArrayList<String>();
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area : areaList) {
            List<StationIdXML> stationList = area.getStationIds();
            for (StationIdXML station : stationList) {
                if (station.getName().equals(stationId)) {
                    results.add(area.getAreaId());
                }
            }
        }
        return results;
    }

    /**
     * Get all the stations associated with the areas.
     * 
     * @return List of stations
     */
    public List<String> getStations() {
        List<AreaIdXML> areaXml = configXml.getAreaIds();
        List<String> stations = new ArrayList<String>();
        for (AreaIdXML area : areaXml) {
            List<StationIdXML> stationList = area.getStationIds();
            for (StationIdXML station : stationList) {
                stations.add(station.getName() + "#" + station.getType() + "#"
                        + area.getAreaId());
            }
        }
        return stations;
    }

    /**
     * Get a list of all monitoring areas.
     * 
     * @return List<String> of monitor area ids
     */
    public List<String> getAreaList() {
        List<AreaIdXML> areaXmlList = configXml.getAreaIds();
        List<String> areaList = new ArrayList<String>();
        for (AreaIdXML area : areaXmlList) {
            areaList.add(area.getAreaId());
        }
        return areaList;
    }

    /**
     * Get a list of all adjacent areas.
     * 
     * @return ArrayList<String> of adjacent area ids
     */
    public List<String> getAdjacentAreaList() {
        List<AreaIdXML> areaXmlList = adjAreaConfigXml.getAreaIds();
        List<String> areaList = new ArrayList<String>();
        for (AreaIdXML area : areaXmlList) {
            areaList.add(area.getAreaId());
        }
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
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML areaXml : areaList) {
            if (areaXml.getAreaId().equals(area)) {
                List<StationIdXML> stationList = areaXml.getStationIds();
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
     *            The station to remove
     */
    public void removeStation(String station) {
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML areaXml : areaList) {
            List<StationIdXML> stationList = areaXml.getStationIds();
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
     *            The area to get
     * @return The AreaIdXML object
     */
    public AreaIdXML getAreaXml(String area) {
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML areaXml : areaList) {
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
     *            The area to remove
     */
    public void removeArea(String area) {
        List<AreaIdXML> areaList = configXml.getAreaIds();
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
     *            The area to remove
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
    public List<String> getAddedZones() {
        return addedZones;
    }

    /**
     * @param addedZones
     *            the addedZones to set
     */
    public void setAddedZones(ArrayList<String> addedZones) {
        this.addedZones = addedZones;
    }

    /**
     * @return the addedStations
     */
    public List<String> getAddedStations() {
        return addedStations;
    }

    /**
     * @param addedStations
     *            the addedStations to set
     */
    public void setAddedStations(ArrayList<String> addedStations) {
        this.addedStations = addedStations;
    }

    // TODO: Include Mesonet data types.
    /**
     * Get station type.
     * 
     * @param theZone
     * @param theStation
     * @return type of station
     */
    public ObsHistType getStationType(String theZone, String theStation) {
        ObsHistType result = null;
        List<AreaIdXML> areaList = configXml.getAreaIds();
        for (AreaIdXML area : areaList) {
            if (area.getAreaId().equals(theZone)) {
                List<StationIdXML> stationList = area.getStationIds();
                for (StationIdXML station : stationList) {
                    if (station.getName().equals(theStation)) {
                        String typeString = station.getType();
                        result = typeString.equals(xmlDataType) ? ObsHistType.METAR
                                : ObsHistType.Maritime;
                        return result;
                    }
                }
            }
        }
        return result;
    }
}
