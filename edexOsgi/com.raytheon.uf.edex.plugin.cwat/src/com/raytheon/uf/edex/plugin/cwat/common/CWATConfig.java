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
package com.raytheon.uf.edex.plugin.cwat.common;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.GraphicBlockValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.monitor.config.CWATLocationConfigManager;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.ThreatLocation;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SCANSiteXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.edex.dat.utils.DATUtils;
import com.raytheon.uf.edex.dat.utils.ScanDataCache;
import com.raytheon.uf.edex.plugin.cwat.CWATGenerator;
import com.raytheon.uf.edex.plugin.cwat.CWATURIFilter;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.raytheon.uf.edex.plugin.scan.process.U500Product;
import com.raytheon.uf.edex.plugin.scan.process.U700Product;
import com.raytheon.uf.edex.plugin.scan.process.V700Product;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * CWATConfig object
 * 
 * Hold config for CWAT Generator
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/02/2009   2037       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class CWATConfig {

    /** CZ */
    private String czURI = null;

    /** VIL */
    private String vilURI = null;

    /** VIL Record */
    private RadarRecord vilRec = null;

    /** CZ Record */
    private RadarRecord czRec = null;

    /** early for MD */
    private RadarRecord mdRec = null;

    /** early for TVS */
    private RadarRecord tvsRec = null;

    /** current binlightning record */
    private ArrayList<BinLightningRecord> lgtRecs = null;

    /** Whether or not to use lightning */
    private boolean useLGT = false;

    /** Whether or not to use MD */
    private boolean useMD = false;

    /** Whether or not to use TVS */
    private boolean useTVS = false;

    /** use model data **/
    private boolean useModel = false;

    /** use upper air data **/
    private boolean useUA = false;

    /** Our generator reference */
    private CWATGenerator cwatgen = null;

    /** Spatial Info object */
    private RadarStation spatialInfo = null;

    /** the latitude/longitude */
    private Coordinate latlon = null;

    /** the icao */
    private String icao = null;

    /** dimensions of the data */
    public static final int dimensions = 2;

    /** wind speed u 700 mb **/
    public GridRecord u700 = null;

    /** wind speed v 700 mb **/
    public GridRecord v700 = null;

    /** U wind at 500 mb **/
    public GridRecord u500 = null;

    public VerticalSounding vs = null;

    /** cell location map **/
    public HashMap<String, Coordinate> cellMap = null;

    /** locations array **/
    public ArrayList<ThreatLocation> locations = new ArrayList<ThreatLocation>();

    /** Cooordinate Reference System **/
    public ProjectedCRS crs = null;

    public Date previousVolScanTime = null;

    /**
     * Constructor
     * 
     * @param genMessage
     * @param cwatgen
     * @throws Exception
     */
    public CWATConfig(URIGenerateMessage genMessage, CWATGenerator cwatgen)
            throws Exception {

        this.cwatgen = cwatgen;

        vilURI = genMessage.getURI(CWATURIFilter.vil);
        czURI = genMessage.getURI(CWATURIFilter.cz);

        setRadarRecords();

        setIcao(getCZ().getIcao());
        setSpatialInfo(getCZ().getSpatialObject());
        setLatLon(getCZ().getLongitude(), getCZ().getLatitude());
        // make the city list if it dosen't exist yet
        setCRS(getCZ().getCRS());
        setLocations(getLatLon(), getIcao(), getCRS());

        checkRadarProduct(CWATURIFilter.cz);

        Map<String, Map<GraphicBlockValues, String>> catMap = RadarRecordUtil
                .parseGraphicBlock(getCZ());
        ArrayList<String> catKeys = new ArrayList<String>();
        // creates the key list
        for (String key : catMap.keySet()) {
            catKeys.add(key);
        }

        setCellMap(catKeys);
        checkLightning();

        if (checkModel()) {
            useModel = true;
        }

        if (checkUA()) {
            useUA = true;
        }

        if (checkRadarProduct(CWATURIFilter.md)) {
            useMD = true;
        }

        if (checkRadarProduct(CWATURIFilter.tvs)) {
            useTVS = true;
        }
    }

    /**
     * gets the Lightning if available
     * 
     * @return
     */
    public ArrayList<BinLightningRecord> getLightning() {
        return lgtRecs;
    }

    /**
     * gets the Vil
     * 
     * @return
     */
    public RadarRecord getVil() {
        return vilRec;
    }

    /**
     * Get CZ
     * 
     * @return
     */
    public RadarRecord getCZ() {
        return czRec;
    }

    /**
     * gets the TVS URI if used
     * 
     * @return
     */
    public RadarRecord getTVS() {
        return tvsRec;
    }

    /**
     * gets the MD URI if used
     * 
     * @return
     */
    public RadarRecord getMD() {
        return mdRec;
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
     * Use TVS or not
     * 
     * @return
     */
    public boolean isTVS() {
        return useTVS;
    }

    /**
     * Use MD or not
     * 
     * @return
     */
    public boolean isMD() {
        return useMD;
    }

    /**
     * Use Model Data
     * 
     * @return
     */
    public boolean isModel() {
        return useModel;
    }

    /**
     * Use Model Data
     * 
     * @return
     */
    public boolean isUA() {
        return useUA;
    }

    /**
     * gets The SCAN generator
     * 
     * @return
     */
    public CWATGenerator getGenerator() {
        return cwatgen;
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
    public void setLatLon(double lon, double lat) {
        latlon = new Coordinate(lon, lat, 0.0);
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
     * get the populated records
     * 
     */
    private void setRadarRecords() throws Exception {
        try {
            vilRec = ScanCommonUtils.getRadarRecord(vilURI);
            czRec = ScanCommonUtils.getRadarRecord(czURI);

        } catch (PluginException pe) {
            cwatgen.logger
                    .error("Couldn't create RadarRecords, metadata mismatch "
                            + pe);
            pe.printStackTrace();
            throw new Exception("Record requests failed, metadata mismatch ",
                    pe);
        }
    }

    /**
     * Checks the params for Lightning
     * 
     * @param filter
     */
    private void checkLightning() {

        ScanDataCache cache = ScanDataCache.getInstance();
        // get most recent strikes data
        if (previousVolScanTime != null) {

            lgtRecs = cache.getLigtningData().getLightningRecords(
                    previousVolScanTime);

            if (lgtRecs == null || lgtRecs.size() == 0) {
                long now = getCZ().getDataTime().getRefTime().getTime();
                long then = previousVolScanTime.getTime();

                // convert to minutes
                int mins = (int) ((now - then) / (60 * 1000));

                String lgtSQL = "select datauri from binlightning where reftime > (now()- interval \'"
                        + mins + " minutes \')";

                try {
                    lgtRecs = new ArrayList<BinLightningRecord>();
                    Object[] obs = cwatgen.dbRequest(lgtSQL);
                    if (obs.length != 0) {
                        for (int i = 0; i < obs.length; i++) {
                            String lgtUri = (String) obs[i];
                            BinLightningRecord rec = ScanUtils
                                    .getLightningRecord(lgtUri);
                            lgtRecs.add(rec);
                            cache.getLigtningData().addLightningRecord(rec);
                        }
                        useLGT = true;
                    }
                } catch (Exception e) {
                    cwatgen.logger.error("Couldn't load BinLightning records. "
                            + e);
                }
            } else {
                useLGT = true;
            }
        }
    }

    /**
     * Checks the params for DMD
     * 
     * @param filter
     */
    private boolean checkRadarProduct(String productcode) {

        // get most recent product code data

        int interval = 10;

        if (previousVolScanTime != null) {
            interval = (int) (((System.currentTimeMillis()) - (previousVolScanTime
                    .getTime())) / (1000.0 * 60.0));
            interval = interval + 1; // eliminate rounding back and missing the
                                     // time
        }

        String sql = "select datauri from radar where productcode = "
                + new Integer(productcode).intValue()
                + " and reftime > (now()- interval \'" + interval
                + " minutes\') order by reftime desc";
        try {
            Object[] obs = cwatgen.dbRequest(sql);
            if (obs.length == 0) {
                return false;
            } else {
                String uri = (String) obs[0];
                RadarRecord rec = ScanCommonUtils.getRadarRecord(uri);
                if (productcode == CWATURIFilter.cz) {
                    previousVolScanTime = rec.getDataTime().getRefTime();
                }
                if (productcode.equals(CWATURIFilter.md)) {
                    mdRec = rec;
                } else if (productcode.equals(CWATURIFilter.tvs)) {
                    tvsRec = rec;
                }
                return true;
            }
        } catch (Exception e) {
            cwatgen.logger.error("Couldn't load Radar record. " + e);
            return false;
        }
    }

    public GridRecord getU700() {
        return u700;
    }

    public GridRecord getV700() {
        return v700;
    }

    public void setU700(GridRecord u700) {
        this.u700 = u700;
    }

    public void setV700(GridRecord v700) {
        this.v700 = v700;
    }

    public GridRecord getU500() {
        return u500;
    }

    public VerticalSounding getSounding() {
        return vs;
    }

    public void setSounding(VerticalSounding vs) {
        this.vs = vs;
    }

    /**
     * See id you can use UA data
     * 
     * @return
     */
    private boolean checkUA() {

        boolean isUA = false;
        String wmo = null;

        wmo = ScanUtils.getNearestWMO(getCZ().getSpatialObject().getStation()
                .getCoordinate());
        ScanDataCache cache = ScanDataCache.getInstance();

        if (wmo != null) {
            VerticalSounding[] vss = ScanCommonUtils.getSoundingRecord(wmo);

            if ((vss != null) && (vss.length > 0)) {

                VerticalSounding vs1 = cache.getSoundingData(icao)
                        .getSoundingRecord("bufrua");

                if (vs1 != null) {
                    if (vss[vss.length - 1].getDataTime().getRefTime()
                            .after(vs1.getDataTime().getRefTime())) {

                        cache.getSoundingData(icao).setSoundingRecord("bufrua",
                                vss[vss.length - 1]);
                        cache.getSoundingData(icao).setEnvironmentalData(
                                vss[vss.length - 1]);
                        setSounding(vss[vss.length - 1]);
                    } else {
                        setSounding(vs1);
                    }
                } else {

                    cache.getSoundingData(icao).setSoundingRecord("bufrua",
                            vss[vss.length - 1]);
                    cache.getSoundingData(icao).setEnvironmentalData(
                            vss[vss.length - 1]);
                    setSounding(vss[vss.length - 1]);
                }
            }
        }

        return isUA;
    }

    public boolean checkModel() {
        boolean isModel = false;

        SCANRunSiteConfigurationManager scanManager = SCANRunSiteConfigurationManager
                .getInstance();
        try {
            scanManager.readConfigXml();
        } catch (SerializationException e) {
            e.printStackTrace();
        }

        if (scanManager.isPopulated()) {

            try {
                SCANSiteXML siteXML = scanManager.getSiteConfig(getIcao());
                SCANModelParameterXML param700U = siteXML
                        .getModelParameter("U700");
                SCANModelParameterXML param700V = siteXML
                        .getModelParameter("V700");
                SCANModelParameterXML param500U = siteXML
                        .getModelParameter("U500");

                // check back for a couple hours
                int interval = 1440;

                u700 = DATUtils.getMostRecentGridRecord(interval,
                        U700Product.getSQL(interval, U700Product.U700),
                        param700U);

                v700 = DATUtils.getMostRecentGridRecord(interval,
                        V700Product.getSQL(interval, V700Product.V700),
                        param700V);

                u500 = DATUtils.getMostRecentGridRecord(interval,
                        U500Product.getSQL(interval, U500Product.U500),
                        param500U);

                if (u700 != null && v700 != null && u500 != null) {
                    isModel = true;
                }
            } catch (Exception e) {
                cwatgen.logger.error("Model parameters not in site config. "
                        + e);
            }
        }

        return isModel;
    }

    /**
     * Queries the DB for locations to match with threats
     * 
     * @param sitePoint
     */
    private void setLocations(Coordinate site, String siteName, ProjectedCRS crs) {

        CWATLocationConfigManager cwatCLCM = CWATLocationConfigManager
                .getInstance();
        cwatCLCM.readConfigXml(site, siteName, crs);
        this.locations = cwatCLCM.getLocations();
    }

    /**
     * Array of cities/sites covered by CWA threat
     * 
     * @return
     */
    public ArrayList<ThreatLocation> getLocations() {
        return locations;
    }

    public void setCellMap(ArrayList<String> catKeys) {

        cellMap = new HashMap<String, Coordinate>();

        for (String cellID : catKeys) {

            Map<GraphicBlockValues, String> stormMap = RadarRecordUtil
                    .parseGraphicBlock(getCZ()).get(cellID);

            String dazm = null;
            String drng = null;
            Coordinate coor = null;
            // azimuth
            if (stormMap.get(GraphicBlockValues.AZIMUTH) != null
                    && !stormMap.get(GraphicBlockValues.AZIMUTH).equals(
                            "UNKNOWN")
                    && !stormMap.get(GraphicBlockValues.AZIMUTH).equals(
                            "NO_DATA")
                    && !stormMap.get(GraphicBlockValues.AZIMUTH)
                            .equals("BLANK")) {

                dazm = stormMap.get(GraphicBlockValues.AZIMUTH);
            }
            // range
            if (stormMap.get(GraphicBlockValues.RANGE) != null
                    && !stormMap.get(GraphicBlockValues.RANGE)
                            .equals("UNKNOWN")
                    && !stormMap.get(GraphicBlockValues.RANGE)
                            .equals("NO_DATA")
                    && !stormMap.get(GraphicBlockValues.RANGE).equals("BLANK")) {

                double range = new Double(
                        stormMap.get(GraphicBlockValues.RANGE));
                drng = new Double(range * ScanUtils.NMI_TO_KM * 1000)
                        .toString();
            }

            if (dazm != null && drng != null) {
                coor = RadarRecordUtil.getAzRangeLatLon(getCZ(), cellID,
                        getCZ().getSpatialObject().getLat(), getCZ()
                                .getSpatialObject().getLon(), dazm, drng);
                cellMap.put(cellID, coor);
            }
        }
    }

    /**
     * Get the map of cell locations
     * 
     * @return
     */
    public HashMap<String, Coordinate> getCellMap() {
        return cellMap;
    }

    /** Projected CRS **/
    public ProjectedCRS getCRS() {
        return crs;
    }

    /** get the CRS **/
    public void setCRS(ProjectedCRS crs) {
        this.crs = crs;
    }

}
