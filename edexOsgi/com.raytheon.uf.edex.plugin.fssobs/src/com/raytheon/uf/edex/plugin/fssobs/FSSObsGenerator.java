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
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager.MonName;
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
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Generates a FSSObs Record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2010            skorolev     Initial creation
 * May 23, 2014 3086       skorolev     Cleaned code.
 * Aug 18, 2014 3530       bclement     removed constructDataURI() call
 * Sep 04, 2014 3220       skorolev     Replaced 3 URI filters with one.
 * Sep 18, 2015 3873       skorolev     Added moving platforms testing.
 * Oct 19, 2015 3841       skorolev     Corrected isNearZone.
 * Nov 12, 2015 3841       dhladky      Augmented Slav's moving platform fix.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsGenerator extends CompositeProductGenerator implements
        MonitorConfigListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsGenerator.class);

    /** Name of composite generator */
    private static final String genName = "FSSObs";

    /** Product */
    private static final String productType = "fssobs";

    /** Stations to filter */
    private final Set<String> allStations = new HashSet<String>();

    public FSSObsMonitorConfigurationManager fogmcm = null;

    public FSSObsMonitorConfigurationManager ssmcm = null;

    public FSSObsMonitorConfigurationManager snowmcm = null;

    /** Zone constant char */
    private static final char Z = 'Z';

    /**
     * Public construction
     */
    public FSSObsGenerator() {
        super(genName, productType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#generateProduct
     * (com.raytheon.edex.urifilter.URIGenerateMessage)
     */
    @Override
    public void generateProduct(URIGenerateMessage genMessage) {

        FSSObsConfig fss_config = null;
        boolean isStationary = true;
        try {
            fss_config = new FSSObsConfig(genMessage, this);
            this.setPluginDao(new FSSObsDAO(productType));
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Couldn't read FSSObs configuration information.", e);
        }
        List<FSSObsRecord> fssRecs = new ArrayList<FSSObsRecord>();
        for (String uri : genMessage.getUris()) {
            // Test if moving platforms are within configuration distance
            if (uri.contains(ObConst.SYNOPTIC_SHIP)
                    || uri.contains(ObConst.DRIFTING_BUOY)
                    || uri.contains(ObConst.SYNOPTIC_MAROB)) {
                isStationary = false;
                try {
                    if (!isNearZone(uri)) {
                        continue;
                    }
                } catch (SpatialException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "URI: "+uri+" could not be checked for Location information.", e);
                    // If the location info is bad.  we don't want it.
                    continue;
                }
            }

            FSSObsRecord fssObsRec = new FSSObsRecord();
            fssObsRec.setIsStationary(isStationary);
            fssObsRec = fss_config.getTableRow(uri);
            FSSObsDataTransform.buildView(fssObsRec);
            fssRecs.add(fssObsRec);
        }

        if (!fssRecs.isEmpty()) {
            this.setPluginDataObjects(fssRecs
                    .toArray(new PluginDataObject[fssRecs.size()]));
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
        if (fogShipDist != 0.0) {
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
        boolean retVal = false;
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
                    statusHandler.handle(Priority.PROBLEM, "Couldn't find marine zone within distance. lon: "+lon+" lat: "+lat+" dist: "+configDist, e);
                }
                if (shipTozone < configDist) {
                    retVal = true;
                }
            }
        }
        return retVal;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#createFilters()
     */
    @Override
    protected void createFilters() {
        filters = new URIFilter[1];
        filters[0] = new FSSObsURIFilter(genName, allStations);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#configureFilters()
     */
    @Override
    protected void configureFilters() {
        statusHandler.handle(Priority.INFO, getGeneratorName()
                + " process Filter Config...");
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#isRunning()
     */
    @Override
    public boolean isRunning() {
        return getConfigManager().getFSSState();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.monitor.events.MonitorConfigListener#configChanged
     * (com.raytheon.uf.common.monitor.events.MonitorConfigEvent)
     */
    @Override
    public void configChanged(MonitorConfigEvent fce) {
        if (fce.getSource() instanceof FSSObsMonitorConfigurationManager) {
            statusHandler
                    .handle(Priority.INFO,
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
            fogmcm = FSSObsMonitorConfigurationManager.getInstance(MonName.fog);
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
            ssmcm = FSSObsMonitorConfigurationManager.getInstance(MonName.ss);
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
                    .getInstance(MonName.snow);
        }
        return snowmcm;
    }

}