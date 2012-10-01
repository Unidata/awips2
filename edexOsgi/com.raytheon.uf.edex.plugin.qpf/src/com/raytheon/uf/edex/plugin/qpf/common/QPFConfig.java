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
package com.raytheon.uf.edex.plugin.qpf.common;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.qpf.QPFUtils;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SCANSiteXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.dat.utils.DATUtils;
import com.raytheon.uf.edex.dat.utils.ScanDataCache;
import com.raytheon.uf.edex.plugin.qpf.QPFGenerator;
import com.raytheon.uf.edex.plugin.qpf.QPFURIFilter;
import com.raytheon.uf.edex.plugin.qpf.QPFURIGenerateMessage;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.raytheon.uf.edex.plugin.scan.process.U700Product;
import com.raytheon.uf.edex.plugin.scan.process.V700Product;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * QPFConfig object
 * 
 * Hold config for QPF Generator
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/17/2009   1981       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class QPFConfig {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QPFConfig.class);

    /** Storm Track Index */
    private String stiURI = null;

    /** early compare URI for VIL */
    private String earlyVilURI = null;

    /** early compare URI for CZ */
    private String earlyCZURI = null;

    /** current compare URI for VIl */
    private String currentVilURI = null;

    /** current compare URI for CZ */
    private String currentCZURI = null;

    /** Storm Track Index */
    private RadarRecord stiRec = null;

    /** early for VIL */
    private RadarRecord earlyVilRec = null;

    /** early for CZ */
    private RadarRecord earlyCZRec = null;

    /** current for VIl */
    private RadarRecord currentVilRec = null;

    /** current for CZ */
    private RadarRecord currentCZRec = null;

    /** current binlightning record */
    private BinLightningRecord lgtRec = null;

    /** VIL SQL */
    private String vilsql = null;

    /** Composite Reflectivity SQL */
    private String czsql = null;

    /** Whether or not to use the STI for wind */
    private boolean useSTI = false;

    /** Whether or not to use lightning */
    private boolean useLGT = false;

    /** Whether or not to use Models */
    private boolean useModel = false;

    /** Our generator reference */
    private QPFGenerator qpfgen = null;

    /** Spatial Info object */
    private RadarStation spatialInfo = null;

    /** the latitude/longitude */
    private Coordinate latlon = null;

    /** The Generate Message */
    private QPFURIGenerateMessage genMessage = null;

    /** the icao */
    private String icao = null;

    /** dimensions of the data */
    public static final int dimensions = 2;

    /** run or don't run QPF **/
    public boolean mode = false;

    private SimpleDateFormat sdf = null;

    /**
     * public constructor, where the work gets done.
     * 
     * @param vilsql
     * @param czsql
     */
    public QPFConfig(QPFURIGenerateMessage genMessage, QPFGenerator qpfgen)
            throws Exception {

        this.qpfgen = qpfgen;
        this.genMessage = genMessage;

        stiURI = genMessage.getURI(QPFURIFilter.sti);
        currentVilURI = genMessage.getURI(QPFURIFilter.vil);
        currentCZURI = genMessage.getURI(QPFURIFilter.cz);

        sdf = new SimpleDateFormat(URIFilter.radarDatePattern);
        sdf.setTimeZone(TimeZone.getTimeZone("Zulu"));

        try {
            if (setIntervals()) {
                if (getCurrentCZ().getOperationalMode() == 2) {
                    setMode(true);
                    setIcao(genMessage.getIcao());
                    setSpatialInfo(getCurrentCZ().getSpatialObject());
                    setLatLon(getCurrentCZ().getLatitude(), getCurrentCZ()
                            .getLongitude());

                    checkSTI();
                    checkLightning();
                } else {
                    statusHandler
                            .handle(Priority.INFO,
                                    "QPF "
                                            + genMessage.getIcao()
                                            + " not run. Not in Precip Mode. RADAR OP Mode: "
                                            + getCurrentCZ()
                                                    .getOperationalMode());
                }
            }
        } catch (Exception e) {
            throw new Exception("QPFConfig:  QPF cannot run....", e);
        }
    }

    /**
     * To run or not to run
     * 
     * @param mode
     */
    public void setMode(boolean mode) {
        this.mode = mode;
    }

    /**
     * get whether to run or not
     * 
     * @return
     */
    public boolean getMode() {
        return mode;
    }

    /**
     * gets the Lighning if available
     * 
     * @return
     */
    public BinLightningRecord getLightning() {
        return lgtRec;
    }

    /**
     * gets the early Vil
     * 
     * @return
     */
    public RadarRecord getEarlyVil() {
        return earlyVilRec;
    }

    /**
     * Gets the current VIL URI
     * 
     * @return
     */
    public RadarRecord getCurrentVil() {
        return currentVilRec;
    }

    /**
     * Get early CZ
     * 
     * @return
     */
    public RadarRecord getEarlyCZ() {
        return earlyCZRec;
    }

    /**
     * gets the current CZ
     * 
     * @return
     */
    public RadarRecord getCurrentCZ() {
        return currentCZRec;
    }

    /**
     * gets the STI URI if used
     * 
     * @return
     */
    public RadarRecord getSTI() {
        return stiRec;
    }

    /**
     * Use STI or not
     * 
     * @return
     */
    public boolean isSTI() {
        return useSTI;
    }

    /**
     * Use lightning or not
     * 
     * @return
     */
    public boolean isLGT() {
        return useLGT;
    }

    /**
     * gets The SCAN generator
     * 
     * @return
     */
    public QPFGenerator getGenerator() {
        return qpfgen;
    }

    /**
     * gets the spatial object
     * 
     * @return
     */
    public RadarStation getSpatialInfo() {
        return spatialInfo;
    }

    /**
     * sets the spatial object
     */
    public void setSpatialInfo(RadarStation spatialinfo) {

        this.spatialInfo = spatialinfo;
    }

    /**
     * Gets the Lat/Lon coord
     * 
     * @return
     */
    public Coordinate getLatLon() {
        return latlon;
    }

    /**
     * set the center lat lon
     * 
     * @param lat
     * @param lon
     */
    public void setLatLon(double lat, double lon) {
        latlon = new Coordinate(lon, lat);
    }

    /**
     * sets the icao for these SCAN values
     * 
     * @param icao
     */
    public void setIcao(String icao) {
        this.icao = icao;
    }

    /**
     * gets the icao for this SCAN
     * 
     * @return
     */
    public String getIcao() {
        return icao;
    }

    /**
     * Find the compare URI's for VIL and CZ
     * 
     * @param vilmap
     * @param czMap
     */
    private void setRadarRecords(HashMap<Date, String> vilmap,
            HashMap<Date, String> czMap) throws Exception {

        // find similar time stamp
        Set<Date> vilKeys = vilmap.keySet();
        Iterator<Date> iter1 = vilKeys.iterator();
        while (iter1.hasNext()) {
            Date vilcal = (Date) iter1.next();
            Set<Date> czKeys = czMap.keySet();
            Iterator<Date> iter2 = czKeys.iterator();
            while (iter2.hasNext()) {
                Date czcal = (Date) iter2.next();
                if (czcal.equals(vilcal)) {
                    earlyVilURI = vilmap.get(vilcal);
                    earlyCZURI = czMap.get(czcal);
                } else if (vilcal.getTime() + 6000 < czcal.getTime()
                        || vilcal.getTime() - 6000 < czcal.getTime()) {
                    // close enough
                    earlyVilURI = vilmap.get(vilcal);
                    earlyCZURI = czMap.get(czcal);
                }
            }
        }

        if (earlyVilURI == null || earlyCZURI == null) {
            throw new Exception("QPFConfig " + icao
                    + ": Radar Record request failed, no previous data.");
        }

        try {
            stiRec = ScanCommonUtils.getRadarRecord(stiURI);
            currentVilRec = ScanCommonUtils.getRadarRecord(currentVilURI);
            currentCZRec = ScanCommonUtils.getRadarRecord(currentCZURI);
            earlyVilRec = ScanCommonUtils.getRadarRecord(earlyVilURI);
            earlyCZRec = ScanCommonUtils.getRadarRecord(earlyCZURI);
        } catch (Exception e) {
            throw new Exception("QPFConfig " + icao
                    + ": Radar Record request failed. ", e);
        }
    }

    /**
     * Get the times from the DB for the volume scan.
     * 
     * @param sql
     * @param filter
     */
    public HashMap<Date, String> getDataVolumeTimes(String sql)
            throws Exception {

        HashMap<Date, String> cals = new HashMap<Date, String>();

        Object[] objects = qpfgen.dbRequest(sql);

        for (int i = 0; i < objects.length; i++) {
            String uri = (String) objects[i];
            cals.put(QPFURIFilter.getTime(uri, sdf), uri);
        }

        return cals;
    }

    /**
     * Checks the params for STI
     * 
     * @param filter
     */
    private void checkSTI() {
        Date stiCal = null;
        Date czCal = null;

        stiCal = QPFURIFilter.getTime(getSTI().getDataURI(), sdf);
        czCal = QPFURIFilter.getTime(getCurrentCZ().getDataURI(), sdf);

        // czCal check the 12 min interval
        if (Math.abs(stiCal.getTime() - czCal.getTime()) < QPFURIFilter.stiInterval) {
            // extract and check for at least 5 storms
            if (getSTI().getProductDependentValue(
                    ScanUtils.SCAN_STI_HALFWORD_INDEX) >= QPFUtils.MIN_STI_CELLS) {
                qpfgen.logger.debug("# of storms: "
                        + getSTI().getProductDependentValue(
                                ScanUtils.SCAN_STI_HALFWORD_INDEX));
                useSTI = true;
            }
        }

        if (!useSTI) {
            try {
                setModelData();
            } catch (Exception e) {
                qpfgen.logger
                        .error("QPF "
                                + icao
                                + ": Failed to lookup Model Data: No model Data available.");
            }
        }
    }

    /**
     * Checks the params for STI
     * 
     * @param genMessage
     */
    private void checkLightning() {
        try {
            ScanDataCache cache = ScanDataCache.getInstance();

            if (cache.getLigtningData() != null) {
                if (cache.getLigtningData().getLightningRecords().size() == 0) {
                    useLGT = false;
                } else {
                    // use only the most recent
                    lgtRec = cache.getLigtningData().getLightningRecords()
                            .get(0);
                    useLGT = true;
                }
            }
        } catch (Exception e) {
            qpfgen.logger.error("Couldn't load BinLightning records. " + e);
            useLGT = false;
        }
    }

    /**
     * Gets the near time interval
     * 
     * @param nearTime
     * @return
     */
    private Timestamp getNearInterval(Date nearTime) {
        return new Timestamp(nearTime.getTime() - (1000 * 60 * 25));
    }

    /**
     * Gets the far time interval
     * 
     * @param farTime
     * @return
     */
    private Timestamp getFarInterval(Date farTime) {
        return new Timestamp(farTime.getTime() - (1000 * 60 * 10));
    }

    /**
     * Check time intervals
     */
    private boolean setIntervals() {

        try {
            Date currentVilTime = QPFURIFilter.getTime(currentVilURI, sdf);
            vilsql = "select datauri from radar where icao = \'"
                    + genMessage.getIcao() + "\' and productcode = "
                    + QPFURIFilter.vil + " and primaryelevationangle = "
                    + (int) QPFURIFilter.tiltAngle + " and reftime > \'"
                    + getNearInterval(currentVilTime).toString()
                    + "\' and reftime < \'"
                    + getFarInterval(currentVilTime).toString() + "\'";
            Date currentCzuTime = QPFURIFilter.getTime(currentCZURI, sdf);
            czsql = "select datauri from radar where icao = \'"
                    + genMessage.getIcao() + "\' and productcode = "
                    + QPFURIFilter.cz + " and primaryelevationangle = "
                    + (int) QPFURIFilter.tiltAngle + " and reftime > \'"
                    + getNearInterval(currentCzuTime).toString()
                    + "\' and reftime < \'"
                    + getFarInterval(currentCzuTime).toString() + "\'";

            setRadarRecords(getDataVolumeTimes(vilsql),
                    getDataVolumeTimes(czsql));
            return true;
        } catch (Exception e) {
            qpfgen.logger
                    .error("QPFConfig: Couldn't create all needed RadarRecords: "
                            + e.getMessage());
            return false;
        }
    }

    /**
     * Set the Model SQL Uses whatever SCAN Config is set to for that parameter
     * if STI is not available
     */
    private void setModelData() throws Exception {
        SCANRunSiteConfigurationManager scanManager = SCANRunSiteConfigurationManager
                .getInstance();
        scanManager.readConfigXml();

        if (scanManager.isPopulated()) {

            SCANSiteXML siteXML = scanManager.getSiteConfig(getIcao());
            SCANModelParameterXML paramXMLU = siteXML.getModelParameter("U700");
            SCANModelParameterXML paramXMLV = siteXML.getModelParameter("V700");

            int interval = 1440;

            GridRecord modelURec = DATUtils.getMostRecentGridRecord(interval,
                    U700Product.getSQL(interval, U700Product.U700), paramXMLU);

            GridRecord modelVRec = DATUtils.getMostRecentGridRecord(interval,
                    V700Product.getSQL(interval, V700Product.V700), paramXMLV);

            if (modelURec != null && modelVRec != null) {
                useModel = true;
            }
        }
    }

    public SimpleDateFormat getSdf() {
        return sdf;
    }

    /**
     * Is model data available
     * 
     * @return
     */
    public boolean isModel() {
        return useModel;
    }

}
