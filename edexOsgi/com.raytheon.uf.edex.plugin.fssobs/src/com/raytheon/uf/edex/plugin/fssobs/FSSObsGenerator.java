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

package com.raytheon.uf.edex.plugin.fssobs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.dat.utils.DatMenuUtil;
import com.raytheon.uf.edex.plugin.fssobs.common.FSSObsConfig;
import org.locationtech.jts.geom.Coordinate;

/**
 * Generates a FSSObs Record.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 26, 2010           skorolev  Initial creation
 * May 23, 2014  3086     skorolev  Cleaned code.
 * Aug 18, 2014  3530     bclement  removed constructDataURI() call
 * Sep 04, 2014  3220     skorolev  Replaced 3 URI filters with one.
 * Sep 18, 2015  3873     skorolev  Added moving platforms testing.
 * Oct 19, 2015  3841     skorolev  Corrected isNearZone.
 * Nov 12, 2015  3841     dhladky   Augmented Slav's moving platform fix.
 * Dec 02, 2015  3873     dhladky   Fixed performance problems, missing params.
 * Jan 04, 2016  5115     skorolev  Added checkThresholds.
 * Aug 13, 2018  6670     randerso  Code cleanup.
 * May 21, 2019  7689     randerso  Refactor handling of FSSObs thresholds
 *
 * </pre>
 *
 * @author skorolev
 */

public class FSSObsGenerator extends CompositeProductGenerator
        implements MonitorConfigListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsGenerator.class);

    /** List of fixed types **/
    protected static final String[] stationaryTypes = new String[] {
            ObConst.METAR, ObConst.MESONET, ObConst.SPECI,
            ObConst.SYNOPTIC_CMAN, ObConst.SYNOPTIC_MOORED_BUOY };

    /** list of moving types **/
    protected static final String[] movingTypes = new String[] {
            ObConst.SYNOPTIC_SHIP, ObConst.DRIFTING_BUOY,
            ObConst.SYNOPTIC_MAROB };

    /** Name of composite generator */
    private static final String genName = "FSSObs";

    /** Product */
    private static final String productType = "fssobs";

    /** Stations to filter */
    private final Set<String> allStations = new HashSet<>();

    private FSSObsMonitorConfigurationManager fogmcm;

    private FSSObsMonitorConfigurationManager ssmcm;

    private FSSObsMonitorConfigurationManager snowmcm;

    private FSSObsAlarmMgr fogAlarmMgr = null;

    private FSSObsAlarmMgr ssAlarmMgr = null;

    private FSSObsAlarmMgr snowAlarmMgr = null;

    /** Zone constant char */
    private static final char Z = 'Z';

    /**
     * Public construction
     */
    public FSSObsGenerator() {
        super(genName, productType);
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {

        FSSObsConfig fss_config = null;

        try {
            fss_config = new FSSObsConfig(genMessage, this);
            this.setPluginDao(new FSSObsDAO(productType));
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't read FSSObs configuration information.", e);
        }
        List<FSSObsRecord> fssRecs = new ArrayList<>();
        for (String uri : genMessage.getUris()) {

            boolean isStationary = true;
            String reportType = null;
            boolean inRange = false;

            // check if moving type
            for (String t : movingTypes) {
                if (uri.contains(t)) {
                    reportType = t;
                    isStationary = false;

                    try {
                        if (isNearZone(uri)) {
                            inRange = true;
                            statusHandler.handle(Priority.INFO,
                                    "===> Moving platform in Range " + uri);
                        }
                    } catch (SpatialException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "URI: " + uri
                                        + " could not be checked for Location information.",
                                e);
                        // If the location info is bad. we don't want it.
                        inRange = false;
                    }
                    break;
                }
            }

            if (isStationary) {
                // determine stationary type
                for (String t : stationaryTypes) {
                    if (uri.contains(t)) {
                        reportType = t;
                        inRange = true;
                        break;
                    }
                }
            }

            // We only want what we know how to decode
            if (reportType != null && inRange) {
                try {
                    FSSObsRecord fssObsRec = fss_config.getTableRow(uri);
                    FSSObsDataTransform.buildView(fssObsRec);
                    fssRecs.add(fssObsRec);
                    checkThresholds(fssObsRec);
                } catch (Exception e) {
                    statusHandler.error("Error building FSSObsRecord", e);
                }
            }
        }

        if (!fssRecs.isEmpty()) {
            this.setPluginDataObjects(
                    fssRecs.toArray(new PluginDataObject[fssRecs.size()]));
            statusHandler.handle(Priority.INFO, "===> Successfully generated "
                    + fssRecs.size() + " records.");
        }
    }

    /**
     * Checks if ship is near monitoring zones and should be included in FSSObs
     * data.
     *
     * @param uri
     *            sfcobs URI
     * @return true if ship is in vicinity of zone
     * @throws SpatialException
     */
    private boolean isNearZone(String uri) throws SpatialException {
        boolean retVal = false;
        String[] items = uri.split(DataURI.SEPARATOR);
        double latShip = Double.parseDouble(items[6]);
        double lonShip = Double.parseDouble(items[7]);

        double ssShipDist = getSSConfig().getShipDistance();
        if (ssShipDist != 0.0) {
            // check SAFSEAS zones
            retVal = checkMarineZones(getSSConfig(), ssShipDist, latShip,
                    lonShip);
        }

        double fogShipDist = getFogConfig().getShipDistance();

        if (fogShipDist != 0.0 && !retVal) {
            // check Fog zones
            retVal = checkMarineZones(getFogConfig(), fogShipDist, latShip,
                    lonShip);
        }

        return retVal;
    }

    /**
     *
     * Test distance between moving platform and marine zone centroid.
     *
     * @param cfg
     *            configuration manager
     * @param configDist
     *            configuration distance
     * @param lat
     *            ship latitude
     * @param lon
     *            ship longitude
     * @return true if distance less configDist
     */
    private boolean checkMarineZones(FSSObsMonitorConfigurationManager cfg,
            double configDist, double lat, double lon) {
        for (String zone : cfg.getAreaList()) {
            if (zone.charAt(2) == Z) {
                // initial distance
                double shipTozone = configDist;
                try {
                    Coordinate coor = MonitorAreaUtils.getZoneCenter(zone);
                    // zone should have center coordinates.
                    if (coor != null) {
                        shipTozone = distance(lat, lon, coor.y, coor.x);
                    } else {
                        // newly added zone
                        AreaIdXML ssXML = cfg.getAreaXml(zone);
                        shipTozone = distance(lat, lon, ssXML.getCLat(),
                                ssXML.getCLon());
                    }
                } catch (SpatialException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Couldn't find marine zone within distance. lon: "
                                    + lon + " lat: " + lat + " dist: "
                                    + configDist,
                            e);
                }
                if (shipTozone < configDist) {
                    // enough just one zone to include data
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if parameters of FSSObs monitors exceed threshold values and sends
     * AlertViz messages.
     *
     * @param fssObsRec
     */
    private void checkThresholds(FSSObsRecord fssObsRec) {
        getFogAlarmMgr().sendAlertVizMsg(fssObsRec);
        getSSAlarmMgr().sendAlertVizMsg(fssObsRec);
        getSnowAlarmMgr().sendAlertVizMsg(fssObsRec);
    }

    @Override
    protected void createFilters() {
        filters = new URIFilter[1];
        filters[0] = new FSSObsURIFilter(genName, allStations);
    }

    @Override
    protected void configureFilters() {
        statusHandler.handle(Priority.INFO,
                getGeneratorName() + " process Filter Config...");
        allStations.addAll(getFogConfig().getStations());
        allStations.addAll(getSSConfig().getStations());
        allStations.addAll(getSnowConfig().getStations());
    }

    /**
     * Sets Product Time.
     *
     * @param filter
     */
    public void setProductTime(URIFilter filter) {
        productTime = new DataTime(filter.getValidTime());
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getFSSState();
    }

    @Override
    public void configChanged(MonitorConfigEvent fce) {
        if (fce.getSource() instanceof FSSObsMonitorConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring FSSObs URI filters...Run Area Config change");
            resetFilters();
            DatMenuUtil dmu = new DatMenuUtil();
            dmu.setDatSite(SiteUtil.getSite());
            dmu.setOverride(true);
            dmu.createMenus();
        }
    }

    /**
     * Distance between two coordinates.
     *
     * @param lat1
     * @param lon1
     * @param lat2
     * @param lon2
     * @return distance in km
     */
    public static double distance(double lat1, double lon1, double lat2,
            double lon2) {
        // Earth's radius of 6378.137 kilometers
        float EarthRadius = 6378.137f;
        double dLat = toRad(lat2 - lat1);
        double dLon = toRad(lon2 - lon1);
        lat1 = toRad(lat1);
        lat2 = toRad(lat2);

        double a = Math.sin(dLat / 2) * Math.sin(dLat / 2) + Math.sin(dLon / 2)
                * Math.sin(dLon / 2) * Math.cos(lat1) * Math.cos(lat2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return EarthRadius * c;
    }

    /**
     * From grad to radian.
     *
     * @param value
     * @return
     */
    private static double toRad(double value) {
        return value * Math.PI / 180;
    }

    /**
     * Gets Fog Monitor Configuration Manager.
     *
     * @return fogmcm
     */
    public FSSObsMonitorConfigurationManager getFogConfig() {
        if (fogmcm == null) {
            fogmcm = FSSObsMonitorConfigurationManager.getInstance(AppName.FOG);
            fogmcm.addListener(this);
        }
        return fogmcm;
    }

    /**
     * Gets Safeseas Monitor Configuration Manager.
     *
     * @return ssmcm
     */
    public FSSObsMonitorConfigurationManager getSSConfig() {
        if (ssmcm == null) {
            ssmcm = FSSObsMonitorConfigurationManager
                    .getInstance(AppName.SAFESEAS);
            ssmcm.addListener(this);
        }
        return ssmcm;
    }

    /**
     * Gets Snow Monitor Configuration Manager.
     *
     * @return snowmcm
     */
    public FSSObsMonitorConfigurationManager getSnowConfig() {
        if (snowmcm == null) {
            snowmcm = FSSObsMonitorConfigurationManager
                    .getInstance(AppName.SNOW);
            snowmcm.addListener(this);
        }
        return snowmcm;
    }

    /**
     * Gets Fog Alarm Manager.
     *
     * @return manager
     */
    private FSSObsAlarmMgr getFogAlarmMgr() {
        if (fogAlarmMgr == null) {
            this.fogAlarmMgr = FSSObsAlarmMgr.getInstance(AppName.FOG);
        }
        return fogAlarmMgr;
    }

    /**
     * Gets SAFESEAS Alarm Manager.
     *
     * @return manager
     */
    private FSSObsAlarmMgr getSSAlarmMgr() {
        if (ssAlarmMgr == null) {
            this.ssAlarmMgr = FSSObsAlarmMgr.getInstance(AppName.SAFESEAS);
        }
        return ssAlarmMgr;
    }

    /**
     * Gets Fog Alarm Manager.
     *
     * @return manager
     */
    private FSSObsAlarmMgr getSnowAlarmMgr() {
        if (snowAlarmMgr == null) {
            this.snowAlarmMgr = FSSObsAlarmMgr.getInstance(AppName.SNOW);
        }
        return snowAlarmMgr;
    }
}