package com.raytheon.uf.edex.plugin.scan;

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
 * Contractor Address:     6825 Pine Street, Suite 144
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Pattern;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableData;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableData;
import com.raytheon.uf.common.dataplugin.scan.data.MDTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ModelData;
import com.raytheon.uf.common.dataplugin.scan.data.RadarData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.SoundingData;
import com.raytheon.uf.common.dataplugin.scan.data.TVSTableData;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SCANSiteXML;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.dat.utils.DATUtils;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.raytheon.uf.edex.plugin.scan.process.CAPEProduct;
import com.raytheon.uf.edex.plugin.scan.process.CompositeReflectivityProduct;
import com.raytheon.uf.edex.plugin.scan.process.DigitalMesoCycloneTabularProduct;
import com.raytheon.uf.edex.plugin.scan.process.GH1000Product;
import com.raytheon.uf.edex.plugin.scan.process.GH500Product;
import com.raytheon.uf.edex.plugin.scan.process.HELIProduct;
import com.raytheon.uf.edex.plugin.scan.process.LightningProduct;
import com.raytheon.uf.edex.plugin.scan.process.MesoCycloneTabularProduct;
import com.raytheon.uf.edex.plugin.scan.process.ScanProduct;
import com.raytheon.uf.edex.plugin.scan.process.StormTrackTabularProduct;
import com.raytheon.uf.edex.plugin.scan.process.TVSTabularProduct;
import com.raytheon.uf.edex.plugin.scan.process.U500Product;
import com.raytheon.uf.edex.plugin.scan.process.U700Product;
import com.raytheon.uf.edex.plugin.scan.process.UAProduct;
import com.raytheon.uf.edex.plugin.scan.process.V700Product;
import com.raytheon.uf.edex.plugin.scan.process.VILProduct;
import com.vividsolutions.jts.geom.Coordinate;

public class ScanURIFilter extends URIFilter {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public double cellTilt = 0.0;

    public double dmdTilt = 0.5;

    public double tvsTilt = 0.0;

    public double mdTilt = 0.0;

    public ScanTableData<? extends ScanTableDataRow> cellData;

    public ScanTableData<? extends ScanTableDataRow> dmdData;

    public ScanTableData<? extends ScanTableDataRow> tvsData;

    public ScanTableData<? extends ScanTableDataRow> mdData;

    public String wmo;

    public String icao;

    public String cwa;

    public String uri = "";

    public Coordinate stationCoor = null;

    public GridGeometry2D stationGeometry = null;

    /** patterns used for matching URI's **/
    private HashMap<String, Pattern> patternKeys = null;

    public ScanProduct cellScanProduct = null;

    public ScanProduct dmdScanProduct = null;

    public ScanProduct tvsScanProduct = null;

    public ScanProduct mdScanProduct = null;

    public List<ScanProduct> processes = null;

    /**
     * When a new record write is needed, this flags which one to create
     */
    public boolean cellIsNew = false;

    public boolean dmdIsNew = false;

    public boolean tvsIsNew = false;

    public boolean mdIsNew = false;

    /** callback to the generator **/
    public ScanGenerator scan = null;

    /** site configurable information store **/
    public SCANSiteXML site = null;

    /** Pattern for dates in radar */
    public static String datePattern = "yyyy-MM-dd_HH:mm:ss.S";

    public ScanURIFilter(String name, ScanGenerator scan) {
        super(name);
        this.scan = scan;
        patternKeys = new HashMap<String, Pattern>();

        logger.debug("SCAN FILTER " + name + " Filter construction...");
        setDataTypes(new String[] { "radar" });
        // this will come from the localization bundle
        setIcao(name);
        setExclude(false);
        // setup for radar
        SimpleDateFormat datef = new SimpleDateFormat(datePattern);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        setDateFormatter(datef);
        setMatchURIs();
        // setup the scan specific location stuff
        setStationGeometry(icao);
        setStationCWA();
        setStationWmo(ScanUtils.getNearestWMO(getStationCoodinate()));
        logger.info("SCAN FILTER: ICAO: " + icao + " CWA: " + cwa + " WMO: "
                + wmo);
        resetData();
        site = scan.getRunConfig().getSiteConfig(name);
        // invoke the "pull strategy" for model and sounding data
        init();
    }

    /** SCAN table Configuration object **/
    public SCANConfig scanConfig = null;

    public String getWmo() {
        return wmo;
    }

    public void setWmo(String wmo) {
        this.wmo = wmo;
    }

    public String getIcao() {
        return icao;
    }

    public void setIcao(String icao) {
        this.icao = icao;
    }

    public Coordinate getStationCoor() {
        return stationCoor;
    }

    public void setStationCoor(Coordinate stationCoor) {
        this.stationCoor = stationCoor;
    }

    @Override
    protected String applyWildCards(String key) {
        return null;
    }

    @Override
    protected String removeWildCards(String key) {
        return null;
    }

    /**
     * Gets the cell tilt angle
     * 
     * @return
     */
    public double getCellTilt() {
        return cellTilt;
    }

    public void setCellTilt(double cellTilt) {
        this.cellTilt = cellTilt;
    }

    /**
     * Gets the dmd tilt angle
     * 
     * @return
     */
    public double getDmdTilt() {
        return dmdTilt;
    }

    public void setDmdTilt(double dmdTilt) {
        this.dmdTilt = dmdTilt;
    }

    /**
     * Gets the tvs tilt angle
     * 
     * @return
     */
    public double getTvsTilt() {
        return tvsTilt;
    }

    public void setTvsTilt(double tvsTilt) {
        this.tvsTilt = tvsTilt;
    }

    /**
     * Gets the md tilt angle
     * 
     * @return
     */
    public double getMdTilt() {
        return mdTilt;
    }

    public void setMdTilt(double mdTilt) {
        this.mdTilt = mdTilt;
    }

    /**
     * Gets the radar station WMO ID
     * 
     * @return
     */
    public String getStationWmo() {
        return wmo;
    }

    public void setStationWmo(String wmo) {
        this.wmo = wmo;
    }

    public ScanTableData<? extends ScanTableDataRow> getData(ScanTables table) {

        ScanTableData<? extends ScanTableDataRow> data = null;

        if (table.equals(ScanTables.CELL)) {
            return cellData;
        } else if (table.equals(ScanTables.DMD)) {
            return dmdData;
        } else if (table.equals(ScanTables.MESO)) {
            return mdData;
        } else if (table.equals(ScanTables.TVS)) {
            return tvsData;
        }
        return data;
    }

    /**
     * same as above except it's a clone of the original
     * 
     * @param table
     * @return
     */
    public ScanTableData<? extends ScanTableDataRow> getCopy(ScanTables table) {

        ScanTableData<? extends ScanTableDataRow> data = null;

        if (table.equals(ScanTables.CELL)) {
            return cellData.copy();
        } else if (table.equals(ScanTables.DMD)) {
            return dmdData.copy();
        } else if (table.equals(ScanTables.MESO)) {
            return mdData.copy();
        } else if (table.equals(ScanTables.TVS)) {
            return tvsData.copy();
        }
        return data;
    }

    public double getTilt(ScanTables type) {
        if (type == ScanTables.CELL) {
            return getCellTilt();
        } else if (type == ScanTables.DMD) {
            return getDmdTilt();
        } else if (type == ScanTables.TVS) {
            return getTvsTilt();
        } else if (type == ScanTables.MESO) {
            return getMdTilt();
        }
        return 0.0;
    }

    /**
     * gets the tableData by time, type
     */
    public ScanTableData<? extends ScanTableDataRow> getClosestTableData(
            ScanTables table, DataTime time) {

        return getData(table);
    }

    /**
     * gets the tableData by time, type
     */
    public ScanTableData<? extends ScanTableDataRow> getRecentTableData(
            ScanTables type) {

        return getData(type);
    }

    /**
     * Sets the home CWA
     */
    public void setStationCWA() {
        this.cwa = ScanProduct.getCWABySpatialQuery(getStationCoodinate());
    }

    /**
     * Gest the home CWA
     * 
     * @return
     */
    public String getCWA() {
        return cwa;
    }

    public void setStationGeometry(String icao) {
        ArrayList<Object> geoms = ScanUtils.getStationGeometry(icao);
        if (geoms != null) {
            this.stationCoor = (Coordinate) geoms.get(0);
            this.stationGeometry = (GridGeometry2D) geoms.get(1);
        }
    }

    public GridGeometry2D getStationGeometry() {
        return stationGeometry;
    }

    public Coordinate getStationCoodinate() {
        return stationCoor;
    }

    /**
     * reset the data structs
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public void resetData() {
        if (dmdData == null) {
            dmdData = new DMDTableData();
        }
        if (cellData == null) {
            cellData = new CellTableData();
        }
        if (tvsData == null) {
            tvsData = new TVSTableData();
        }
        if (mdData == null) {
            mdData = new MDTableData();
        }

        processes = new ArrayList<ScanProduct>();
    }

    /**
     * Get the generator
     * 
     * @return
     */
    public ScanGenerator getGenerator() {
        return scan;
    }

    /**
     * Gathers the patterns the tables search for
     * 
     * @return
     */
    @Override
    public void setMatchURIs() {

        SCANSiteXML site = scan.getRunConfig().getSiteConfig(getIcao());

        Pattern capep = CAPEProduct.getPattern(site.getModelParameter(
                CAPEProduct.cape).getModelName());
        getMatchURIs().put(capep, 0l);
        patternKeys.put(CAPEProduct.cape, capep);

        Pattern helip = HELIProduct.getPattern(site.getModelParameter(
                HELIProduct.heli).getModelName());
        getMatchURIs().put(helip, 0l);
        patternKeys.put(HELIProduct.heli, helip);

        Pattern GH1000p = GH1000Product.getPattern(site.getModelParameter(
                GH1000Product.GH1000).getModelName());
        getMatchURIs().put(GH1000p, 0l);
        patternKeys.put(GH1000Product.GH1000, GH1000p);

        Pattern GH500p = GH500Product.getPattern(site.getModelParameter(
                GH500Product.GH500).getModelName());
        getMatchURIs().put(GH500p, 0l);
        patternKeys.put(GH500Product.GH500, GH500p);

        Pattern U500p = U500Product.getPattern(site.getModelParameter(
                U500Product.U500).getModelName());
        getMatchURIs().put(U500p, 0l);
        patternKeys.put(U500Product.U500, U500p);

        Pattern U700p = U700Product.getPattern(site.getModelParameter(
                U700Product.U700).getModelName());
        getMatchURIs().put(U700p, 0l);
        patternKeys.put(U700Product.U700, U700p);

        Pattern V700p = V700Product.getPattern(site.getModelParameter(
                V700Product.V700).getModelName());
        getMatchURIs().put(V700p, 0l);
        patternKeys.put(V700Product.V700, V700p);

        getMatchURIs().put(
                StormTrackTabularProduct.getPattern(site.getScanSite(),
                        getCellTilt()), 0l);
        patternKeys.put(StormTrackTabularProduct.sti, StormTrackTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()));
        getMatchURIs()
                .put(DigitalMesoCycloneTabularProduct.getPattern(site
                        .getScanSite()), 0l);
        patternKeys
                .put(DigitalMesoCycloneTabularProduct.dmd,
                        DigitalMesoCycloneTabularProduct.getPattern(site
                                .getScanSite()));
        getMatchURIs()
                .put(TVSTabularProduct.getPattern(site.getScanSite(),
                        getCellTilt()), 0l);
        patternKeys
                .put(TVSTabularProduct.tvs, TVSTabularProduct.getPattern(
                        site.getScanSite(), getCellTilt()));
        getMatchURIs().put(
                MesoCycloneTabularProduct.getPattern(site.getScanSite(),
                        getCellTilt()), 0l);
        patternKeys.put(MesoCycloneTabularProduct.md, MesoCycloneTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()));
        getMatchURIs().put(
                CompositeReflectivityProduct.getPattern(site.getScanSite(),
                        getCellTilt()), 0l);
        patternKeys.put(CompositeReflectivityProduct.cz,
                CompositeReflectivityProduct.getPattern(site.getScanSite(),
                        getCellTilt()));
        getMatchURIs().put(LightningProduct.getPattern(), 0l);
        patternKeys.put(LightningProduct.BINLIGHTNING,
                LightningProduct.getPattern());
        getMatchURIs().put(
                VILProduct.getPattern(site.getScanSite(), getCellTilt()), 0l);
        patternKeys.put(VILProduct.vil,
                VILProduct.getPattern(site.getScanSite(), getCellTilt()));
        // FIXME fix me
        getMatchURIs().put(UAProduct.getPattern(getWmo()), 0l);
        patternKeys.put(UAProduct.UA, UAProduct.getPattern(getWmo()));
    }

    /**
     * In the case of SCAN we match on any URI matching that comes in
     * 
     * @param message
     * @return boolean
     */
    @Override
    public boolean isMatched(DataURINotificationMessage message) {

        setCurrentTime(new Date(System.currentTimeMillis()));

        // process all the product messages
        if ((message != null) && (message.getDataURIs().length > 0)) {
            for (String dataUri : message.getDataURIs()) {
                // check for the needed data types
                if (dataUri.startsWith("/grid") || dataUri.startsWith("/radar")
                        || dataUri.startsWith("/binlightning")
                        || dataUri.startsWith("/bufrua")) {
                    // add your pattern checks to the key
                    for (Pattern pattern : getMatchURIs().keySet()) {
                        if (pattern.matcher(dataUri).matches()) {
                            // matches one of them, which one?
                            String matchKey = getPatternName(pattern);

                            if (matchKey
                                    .equals(CompositeReflectivityProduct.cz)) {
                                cellScanProduct = (new CompositeReflectivityProduct(
                                        dataUri, ScanTables.CELL, this));
                                cellIsNew = true;
                                break;
                            } else if (matchKey
                                    .equals(StormTrackTabularProduct.sti)) {
                                processes.add(new StormTrackTabularProduct(
                                        dataUri, ScanTables.CELL, this));
                                break;
                            } else if (matchKey
                                    .equals(DigitalMesoCycloneTabularProduct.dmd)) {
                                dmdScanProduct = (new DigitalMesoCycloneTabularProduct(
                                        dataUri, ScanTables.DMD, this));
                                dmdIsNew = true;
                                break;
                            } else if (matchKey.equals(TVSTabularProduct.tvs)) {

                                tvsScanProduct = (new TVSTabularProduct(
                                        dataUri, ScanTables.TVS, this));
                                tvsIsNew = true;
                                break;
                            } else if (matchKey
                                    .equals(MesoCycloneTabularProduct.md)) {
                                mdScanProduct = (new MesoCycloneTabularProduct(
                                        dataUri, ScanTables.MESO, this));
                                mdIsNew = true;
                                break;
                            } else if (matchKey
                                    .equals(LightningProduct.BINLIGHTNING)) {
                                processes.add(new LightningProduct(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(VILProduct.vil)) {
                                processes.add(new VILProduct(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(UAProduct.UA)) {
                                processes.add(new UAProduct(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(CAPEProduct.cape)) {
                                processes.add(new CAPEProduct(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(HELIProduct.heli)) {
                                processes.add(new HELIProduct(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(GH1000Product.GH1000)) {
                                processes.add(new GH1000Product(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(GH500Product.GH500)) {
                                processes.add(new GH500Product(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(U500Product.U500)) {
                                processes.add(new U500Product(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(U700Product.U700)) {
                                processes.add(new U700Product(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(V700Product.V700)) {
                                processes.add(new V700Product(dataUri,
                                        ScanTables.CELL, this));
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Process the products
        synchronized (processes) {
            for (ScanProduct sp : processes) {
                if (!sp.getAllowNew()) {
                    scan.getExecutor().execute(new ProcessProduct(sp));
                }
            }
        }

        // wait for all threads to finish
        if (processes.size() > 0) {
            int limit = 1000;
            while (processes.size() > 0) {
                // wait for all threads to finish before returning
                try {
                    if (limit > 0) {
                        Thread.sleep(10);
                        limit--;
                    } else {
                        synchronized (processes) {
                            processes.clear();
                        }
                        logger.error("Orphaned Product processing, to much time ..."
                                + processes.size());
                    }
                } catch (InterruptedException e) {
                    synchronized (processes) {
                        processes.clear();
                    }
                    logger.error("Entire product array processing failed....");
                }
            }
        }

        if (tvsIsNew) {
            try {
                tvsScanProduct.process();
            } catch (Exception e) {
                logger.error("TVS product processing did not complete....", e);
                tvsIsNew = false;
            }
        }
        if (mdIsNew) {
            try {
                mdScanProduct.process();
            } catch (Exception e) {
                logger.error("MD product processing did not complete....", e);
                mdIsNew = false;
            }
        }
        if (dmdIsNew) {
            try {
                dmdScanProduct.process();
            } catch (Exception e) {
                logger.error("DMD product processing did not complete....", e);
                dmdIsNew = false;
            }
        }
        if (cellIsNew) {
            try {
                cellScanProduct.process();
            } catch (Exception e) {
                logger.error("CELL product processing did not complete....", e);
                e.printStackTrace();
                cellIsNew = false;
            }
        }

        if (cellIsNew || dmdIsNew || tvsIsNew || mdIsNew) {
            match = true;
        }

        return match;
    }

    /**
     * reset after a successful match
     */
    @Override
    public void reset() {
        match = false;
        cellScanProduct = null;
        dmdScanProduct = null;
        tvsScanProduct = null;
        mdScanProduct = null;
        cellIsNew = false;
        dmdIsNew = false;
        tvsIsNew = false;
        mdIsNew = false;
        processes.clear();
    }

    /**
     * gets the matching key for a matching pattern
     * 
     * @param pattern
     * @return
     */
    private String getPatternName(Pattern pattern) {
        for (String key : patternKeys.keySet()) {
            if (patternKeys.get(key).pattern().equals(pattern.toString())) {
                return key;
            }
        }

        return null;
    }

    /**
     * Determines if a cell record needs to be closed
     * 
     * @return
     */
    public boolean cellIsNew() {
        return cellIsNew;
    }

    /**
     * Determines if a dmd record needs to be closed
     * 
     * @return
     */
    public boolean dmdIsNew() {
        return dmdIsNew;
    }

    /**
     * Determines if a tvs record needs to be closed
     * 
     * @return
     */
    public boolean tvsIsNew() {
        return tvsIsNew;
    }

    /**
     * Determines if an md record needs to be closed
     * 
     * @return
     */
    public boolean mdIsNew() {
        return mdIsNew;
    }

    /**
     * Get the SoundingData
     * 
     * If there is no data yet in the map then use a "pull strategy" to obtain
     * this data; but only do this when there is no data available yet.
     * 
     * @return
     */
    public SoundingData getSoundingData() {
        return scan.getCache().getSoundingData(getIcao());
    }

    /**
     * Get the Radar Data
     * 
     * @return the radar data
     */
    public RadarData getRadarData() {
        return scan.getCache().getRadarData(getIcao());
    }

    /**
     * Gets the grib record by URI
     * 
     * @return
     * @throws Exception
     */
    private GridRecord getRecordGrib(String guri) throws Exception {

        GridRecord grib = null;

        try {
            if (!guri.isEmpty()) {
                grib = DATUtils.getGridRecord(guri);
            }

        } catch (Exception e) {
            logger.debug("Grib record data un-available.....");
        }

        return grib;
    }

    /**
     * Gets the radar record by URI
     * 
     * @return
     * @throws Exception
     */
    private RadarRecord getRecordRadar(String ruri) throws Exception {

        RadarRecord radar = null;

        try {
            radar = ScanCommonUtils.getRadarRecord(ruri);

        } catch (Exception e) {
            logger.debug("RADAR record data un-available.....");
        }

        return radar;
    }

    /**
     * Gets the sounding record by URI
     * 
     * @return
     * @throws Exception
     */
    private VerticalSounding[] getRecordSounding(String suri) throws Exception {

        VerticalSounding[] sounding = null;

        try {
            if (!suri.isEmpty()) {
                sounding = ScanUtils.getSoundingRecord(suri);
            } else {
                sounding = ScanCommonUtils.getSoundingRecord(getWmo());
            }

        } catch (Exception e) {
            logger.debug("No UA record data available.....");
            // e.printStackTrace();
        }

        return sounding;
    }

    /**
     * Set Grib Record in Model Data
     */
    public void setGridRecord(String prodType, String guri) {

        String modelName = site.getModelParameter(prodType).getModelName();

        try {
            scan.getCache().getModelData()
                    .setGridRecord(modelName, prodType, getRecordGrib(guri));
        } catch (Exception e) {
            logger.debug("Grib record setter failed.....");
        }
    }

    /**
     * Set Grib Record in Model Data
     */
    public void setGridRecord(String prodType, GridRecord rec) {

        String modelName = site.getModelParameter(prodType).getModelName();

        try {
            scan.getCache().getModelData()
                    .setGridRecord(modelName, prodType, rec);
        } catch (Exception e) {
            logger.debug("Grib record setter failed.....");
        }
    }

    /**
     * Gets model data
     * 
     * @return
     */
    public ModelData getModelData() {
        return scan.getCache().getModelData();
    }

    /**
     * Set Grib Record in Model Data
     */
    public void setGridRecords() {
        try {
            GridRecord[] records = { null, null, null, null, null, null, null,
                    null };
            records = getGridRecords();
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(CAPEProduct.cape)
                                    .getModelName(), CAPEProduct.cape,
                            records[0]);
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(HELIProduct.heli)
                                    .getModelName(), HELIProduct.heli,
                            records[1]);
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(U500Product.U500)
                                    .getModelName(), U500Product.U500,
                            records[2]);
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(U700Product.U700)
                                    .getModelName(), U700Product.U700,
                            records[3]);
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(V700Product.V700)
                                    .getModelName(), V700Product.V700,
                            records[4]);
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(GH500Product.GH500)
                                    .getModelName(), GH500Product.GH500,
                            records[5]);
            scan.getCache()
                    .getModelData()
                    .setGridRecord(
                            site.getModelParameter(GH1000Product.GH1000)
                                    .getModelName(), GH1000Product.GH1000,
                            records[6]);
        } catch (Exception e) {
            logger.debug("Grib record setter failed.....");
        }
    }

    /**
     * Get Grib Record from Model Data
     */
    public GridRecord getGridRecord(String prodType) {
        GridRecord rec = null;
        String modelName = site.getModelParameter(prodType).getModelName();
        try {
            rec = scan.getCache().getModelData()
                    .getGridRecord(modelName, prodType);
        } catch (Exception e) {
            logger.debug("No Grib Data available.....");
        }
        return rec;
    }

    /**
     * Set Radar Record in Radar Data
     */
    public void setRadarRecord(String prodType, String ruri) {
        try {
            scan.getCache().getRadarData(getIcao())
                    .setRadarRecord(prodType, getRecordRadar(ruri));
        } catch (Exception e) {
            logger.debug("Radar record setter failed.....");
        }
    }

    /**
     * Get Radar Record from Radar Data
     */
    public RadarRecord getRadarRecord(String prodType) {
        RadarRecord rec = null;
        try {
            rec = scan.getCache().getRadarData(getIcao())
                    .getRadarRecord(prodType);
        } catch (Exception e) {
            logger.debug("No Radar Data available.....");
        }
        return rec;
    }

    /**
     * Set Sounding Record in Sounding Data
     */
    public void setSoundingRecord(String prodType, String suri) {
        try {
            VerticalSounding[] vs = getRecordSounding(suri);
            if ((vs != null) && (vs.length > 0)) {
                getSoundingData()
                        .setSoundingRecord(prodType, vs[vs.length - 1]);
                getSoundingData().setEnvironmentalData(vs[vs.length - 1]);
            }
        } catch (Exception e) {
            logger.error("UA record setter failed.....");
        }
    }

    /**
     * Get Sounding Record from Sounding Data
     */
    public VerticalSounding getSoundingRecord(String prodType) {
        VerticalSounding rec = null;
        try {
            rec = getSoundingData().getSoundingRecord(prodType);
        } catch (Exception e) {
            logger.debug("No UA Data available.....");
        }
        return rec;
    }

    /**
     * Get lightning data
     * 
     * @param date
     * @return
     */
    public ArrayList<BinLightningRecord> getLightningData(Date date) {
        return scan.getCache().getLigtningData().getLightningRecords(date);
    }

    /**
     * checks for data
     * 
     * @param date
     * @return
     */
    public boolean isLighting(Date date) {
        return scan.getCache().getLigtningData().hasRecords(date);
    }

    /**
     * Add lightning record
     * 
     * @param record
     */
    public void setLightningData(BinLightningRecord record) {
        scan.getCache().getLigtningData().addLightningRecord(record);
    }

    /**
     * Initializes model data and sounding data on demand using a pull strategy
     * to retrieve the data from a database.
     */
    public void init() {
        setGridRecords();
        setSoundingRecord(ScanProduct.UA, "");
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new ScanURIGenerateMessage(this);
    }

    /**
     * Inner class to thread the scan processing
     * 
     * @author dhladky
     * 
     */
    class ProcessProduct implements Runnable {

        ScanProduct scanProduct = null;

        @Override
        public void run() {
            logger.debug("ProcessProduct: Starting thread " + scanProduct.uri);
            process();
            logger.debug("ProcessProduct: Finishing thread " + scanProduct.uri);
        }

        public ProcessProduct(ScanProduct scanProduct) {
            this.scanProduct = scanProduct;
        }

        public void process() {
            try {
                long time = System.currentTimeMillis();
                scanProduct.process();
                long time2 = System.currentTimeMillis();
                logger.debug("SCAN took: " + (time2 - time) + " ms");
            } catch (Exception e) {
                logger.error("ProcessProduct: did not complete....." + e);
            } finally {
                synchronized (processes) {
                    processes.remove(scanProduct);
                }
            }
        }
    }

    public String getFullPathAndFileName() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("scan").append(fs).append("ScanAlarms.xml");

        return sb.toString();
    }

    public ScanAlarmXML getAlarmData() {
        ScanAlarmXML dataXML = null;

        try {
            dataXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            String path = pm.getStaticFile(getFullPathAndFileName())
                    .getAbsolutePath();

            dataXML = (ScanAlarmXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path);
        } catch (Exception e) {
            logger.error("Scan Alaram data not available");
            dataXML = null;
        }

        return dataXML;
    }

    /**
     * get Populated grib records for CAPE and HELI
     * 
     * @return
     */
    public GridRecord[] getGridRecords() throws PluginException {

        GridRecord[] records = { null, null, null, null, null, null, null, null };
        try {
            String[] modelUris = getModelSQL();
            // CAPE
            records[0] = DATUtils.getGridRecord(modelUris[0]);
            // HELI
            records[1] = DATUtils.getGridRecord(modelUris[1]);
            // U500
            records[2] = DATUtils.getGridRecord(modelUris[2]);
            // U700
            records[3] = DATUtils.getGridRecord(modelUris[3]);
            // V700
            records[4] = DATUtils.getGridRecord(modelUris[4]);
            // GH500
            records[5] = DATUtils.getGridRecord(modelUris[5]);
            // GH1000
            records[6] = DATUtils.getGridRecord(modelUris[6]);
        } catch (Exception e) {
            logger.error("No Grib record(s) found.....");
        }
        return records;
    }

    /**
     * Get the Model SQL Uses the environmental data backup for the
     * "pull strategy" for All model params.
     */
    private String[] getModelSQL() throws Exception {

        SCANSiteXML site = scan.getRunConfig().getSiteConfig(getIcao());
        int interval = 1440;
        // Set interval to 1 day, 1440 minutes
        Object[] objectsCapeUri = scan.dbRequest(CAPEProduct.getSQL(interval,
                site.getModelParameter(CAPEProduct.cape).getModelName()));
        Object[] objectsHeliUri = scan.dbRequest(HELIProduct.getSQL(interval,
                site.getModelParameter(HELIProduct.heli).getModelName()));
        Object[] objectsU500Uri = scan.dbRequest(U500Product.getSQL(interval,
                site.getModelParameter(U500Product.U500).getModelName()));
        Object[] objectsU700Uri = scan.dbRequest(U700Product.getSQL(interval,
                site.getModelParameter(U700Product.U700).getModelName()));
        Object[] objectsV700Uri = scan.dbRequest(V700Product.getSQL(interval,
                site.getModelParameter(V700Product.V700).getModelName()));
        Object[] objectsGH500Uri = scan.dbRequest(GH500Product.getSQL(interval,
                site.getModelParameter(GH500Product.GH500).getModelName()));
        Object[] objectsGH1000Uri = scan.dbRequest(GH1000Product.getSQL(
                interval, site.getModelParameter(GH1000Product.GH1000)
                        .getModelName()));

        // always grab the most recent time data
        String[] results = { "", "", "", "", "", "", "", "" };
        results[0] = (String) objectsCapeUri[0];
        results[1] = (String) objectsHeliUri[0];
        results[2] = (String) objectsU500Uri[0];
        results[3] = (String) objectsU700Uri[0];
        results[4] = (String) objectsV700Uri[0];
        results[5] = (String) objectsGH500Uri[0];
        results[6] = (String) objectsGH1000Uri[0];
        return results;
    }

    /**
     * get my model param
     * 
     * @param paramName
     * @return
     */
    public SCANModelParameterXML getModelParameter(String paramName) {
        SCANModelParameterXML param = null;

        if (site.getModelParameter(paramName) != null) {
            param = site.getModelParameter(paramName);
        }

        return param;
    }
}
