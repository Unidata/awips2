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
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;
import java.util.regex.Pattern;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
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
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SCANSiteXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.time.util.TimeUtil;
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
import org.locationtech.jts.geom.Coordinate;

/**
 *
 * Filters URIs for SCAN processing
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2010            dhladky     Initial creation
 * Jun 21, 2013 7613       zhao        Modified setGridRecords() etc.
 * Oct 15, 2013 2361       njensen     Use JAXBManager for XML
 * Apr 24, 2014 2060       njensen     Updates for removal of grid dataURI column
 * May 12, 2014 3133       njensen     Remove unused field
 * Jan 20, 2014 3949       nabowle     Only match binlightning uris with the proper
 *                                     lightning source.
 * Aug 31, 2017 6408       njensen     Keep radar product valid times separate
 * Apr 04, 2018 6696       randerso    Code cleanup
 *
 *
 * </pre>
 *
 * @author dhladky
 */
public class ScanURIFilter extends URIFilter {

    private static final SingleTypeJAXBManager<ScanAlarmXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(ScanAlarmXML.class);

    /** Pattern for dates in radar */
    public static final String DATE_PATTERN = "yyyy-MM-dd_HH:mm:ss.S";

    private double cellTilt = 0.0;

    private double dmdTilt = 0.5;

    private double tvsTilt = 0.0;

    private double mdTilt = 0.0;

    private ScanTableData<? extends ScanTableDataRow> cellData;

    private ScanTableData<? extends ScanTableDataRow> dmdData;

    private ScanTableData<? extends ScanTableDataRow> tvsData;

    private ScanTableData<? extends ScanTableDataRow> mdData;

    private String wmo;

    private String icao;

    private String cwa;

    private Coordinate stationCoor = null;

    private GridGeometry2D stationGeometry = null;

    /** patterns used for matching URI's **/
    private Map<String, Pattern> patternKeys = null;

    private ScanProduct cellScanProduct = null;

    private ScanProduct dmdScanProduct = null;

    private ScanProduct tvsScanProduct = null;

    private ScanProduct mdScanProduct = null;

    private List<ScanProduct> processes = null;

    /*
     * When a new record write is needed, this flags which one to create
     */
    private boolean cellIsNew = false;

    private boolean dmdIsNew = false;

    private boolean tvsIsNew = false;

    private boolean mdIsNew = false;

    /*
     * DR 6408: Keep the times for each product separate
     */
    private Date cellValidTime = null;

    private Date dmdValidTime = null;

    private Date tvsValidTime = null;

    private Date mdValidTime = null;

    /** callback to the generator **/
    private ScanGenerator scan = null;

    /** site configurable information store **/
    private SCANSiteXML site = null;

    public ScanURIFilter(String name, ScanGenerator scan) {
        super(name);
        this.scan = scan;
        patternKeys = new HashMap<>();

        logger.debug("SCAN FILTER " + name + " Filter construction.");
        setDataTypes(new String[] { "radar" });
        // this will come from the localization bundle
        setIcao(name);
        setExclude(false);
        // setup for radar
        SimpleDateFormat datef = new SimpleDateFormat(DATE_PATTERN);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        setDateFormatter(datef);
        setMatchURIs();
        // setup the scan specific location stuff
        setStationGeometry(icao);
        setStationCWA();
        setStationWmo(ScanUtils.getNearestWMO(getStationCoodinate()));
        logger.info(
                "SCAN FILTER: ICAO: " + icao + " CWA: " + cwa + " WMO: " + wmo);
        resetData();
        site = scan.getRunConfig().getSiteConfig(name);
        // invoke the "pull strategy" for model and sounding data
        init();
    }

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

    public double getCellTilt() {
        return cellTilt;
    }

    public void setCellTilt(double cellTilt) {
        this.cellTilt = cellTilt;
    }

    public double getDmdTilt() {
        return dmdTilt;
    }

    public void setDmdTilt(double dmdTilt) {
        this.dmdTilt = dmdTilt;
    }

    public double getTvsTilt() {
        return tvsTilt;
    }

    public void setTvsTilt(double tvsTilt) {
        this.tvsTilt = tvsTilt;
    }

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
     * Sets the home CWA
     */
    public void setStationCWA() {
        this.cwa = ScanProduct.getCWABySpatialQuery(getStationCoodinate());
    }

    /**
     * Gets the home CWA
     *
     * @return
     */
    public String getCWA() {
        return cwa;
    }

    public void setStationGeometry(String icao) {
        List<Object> geoms = ScanUtils.getStationGeometry(icao);
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

        processes = new ArrayList<>();
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
     */
    @Override
    public void setMatchURIs() {
        SCANSiteXML site = scan.getRunConfig().getSiteConfig(getIcao());

        Pattern capep = CAPEProduct.getPattern(
                site.getModelParameter(CAPEProduct.cape).getModelName());
        getMatchURIs().put(capep, 0l);
        patternKeys.put(CAPEProduct.cape, capep);

        Pattern helip = HELIProduct.getPattern(
                site.getModelParameter(HELIProduct.heli).getModelName());
        getMatchURIs().put(helip, 0l);
        patternKeys.put(HELIProduct.heli, helip);

        Pattern GH1000p = GH1000Product.getPattern(
                site.getModelParameter(GH1000Product.GH1000).getModelName());
        getMatchURIs().put(GH1000p, 0l);
        patternKeys.put(GH1000Product.GH1000, GH1000p);

        Pattern GH500p = GH500Product.getPattern(
                site.getModelParameter(GH500Product.GH500).getModelName());
        getMatchURIs().put(GH500p, 0l);
        patternKeys.put(GH500Product.GH500, GH500p);

        Pattern U500p = U500Product.getPattern(
                site.getModelParameter(U500Product.U500).getModelName());
        getMatchURIs().put(U500p, 0l);
        patternKeys.put(U500Product.U500, U500p);

        Pattern U700p = U700Product.getPattern(
                site.getModelParameter(U700Product.U700).getModelName());
        getMatchURIs().put(U700p, 0l);
        patternKeys.put(U700Product.U700, U700p);

        Pattern V700p = V700Product.getPattern(
                site.getModelParameter(V700Product.V700).getModelName());
        getMatchURIs().put(V700p, 0l);
        patternKeys.put(V700Product.V700, V700p);

        getMatchURIs().put(StormTrackTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()), 0l);
        patternKeys.put(StormTrackTabularProduct.sti, StormTrackTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()));
        getMatchURIs().put(
                DigitalMesoCycloneTabularProduct.getPattern(site.getScanSite()),
                0l);
        patternKeys.put(DigitalMesoCycloneTabularProduct.DMD,
                DigitalMesoCycloneTabularProduct
                        .getPattern(site.getScanSite()));
        getMatchURIs().put(
                TVSTabularProduct.getPattern(site.getScanSite(), getCellTilt()),
                0l);
        patternKeys.put(TVSTabularProduct.TVS, TVSTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()));
        getMatchURIs().put(MesoCycloneTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()), 0l);
        patternKeys.put(MesoCycloneTabularProduct.MD, MesoCycloneTabularProduct
                .getPattern(site.getScanSite(), getCellTilt()));
        getMatchURIs().put(CompositeReflectivityProduct
                .getPattern(site.getScanSite(), getCellTilt()), 0l);
        patternKeys.put(CompositeReflectivityProduct.CZ,
                CompositeReflectivityProduct.getPattern(site.getScanSite(),
                        getCellTilt()));
        Pattern lightningPattern = LightningProduct
                .getPattern(SCANRunSiteConfigurationManager.getInstance()
                        .getLightningSource());
        getMatchURIs().put(lightningPattern, 0l);
        patternKeys.put(LightningProduct.BINLIGHTNING, lightningPattern);
        getMatchURIs().put(
                VILProduct.getPattern(site.getScanSite(), getCellTilt()), 0l);
        patternKeys.put(VILProduct.vil,
                VILProduct.getPattern(site.getScanSite(), getCellTilt()));

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
                                    .equals(CompositeReflectivityProduct.CZ)) {
                                cellScanProduct = (new CompositeReflectivityProduct(
                                        dataUri, ScanTables.CELL, this));
                                cellIsNew = true;
                                break;
                            } else if (matchKey
                                    .equals(StormTrackTabularProduct.sti)) {
                                processes.add(new StormTrackTabularProduct(
                                        dataUri, ScanTables.CELL, this));
                                break;
                            } else if (matchKey.equals(
                                    DigitalMesoCycloneTabularProduct.DMD)) {
                                dmdScanProduct = (new DigitalMesoCycloneTabularProduct(
                                        dataUri, ScanTables.DMD, this));
                                dmdIsNew = true;
                                break;
                            } else if (matchKey.equals(TVSTabularProduct.TVS)) {

                                tvsScanProduct = (new TVSTabularProduct(dataUri,
                                        ScanTables.TVS, this));
                                tvsIsNew = true;
                                break;
                            } else if (matchKey
                                    .equals(MesoCycloneTabularProduct.MD)) {
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
        if (!processes.isEmpty()) {
            int limit = 1000;
            while (!processes.isEmpty()) {
                // wait for all threads to finish before returning
                try {
                    if (limit > 0) {
                        Thread.sleep(10);
                        limit--;
                    } else {
                        logger.error("Products took too long to process, "
                                + processes.size() + " remaining, aborting");
                        synchronized (processes) {
                            processes.clear();
                        }
                    }
                } catch (InterruptedException e) {
                    synchronized (processes) {
                        processes.clear();
                    }
                    logger.error("Entire product array processing failed.", e);
                }
            }
        }

        if (tvsIsNew) {
            try {
                tvsScanProduct.process();
            } catch (Exception e) {
                logger.error("TVS product processing did not complete.", e);
                tvsIsNew = false;
            }
        }
        if (mdIsNew) {
            try {
                mdScanProduct.process();
            } catch (Exception e) {
                logger.error("MD product processing did not complete.", e);
                mdIsNew = false;
            }
        }
        if (dmdIsNew) {
            try {
                dmdScanProduct.process();
            } catch (Exception e) {
                logger.error("DMD product processing did not complete.", e);
                dmdIsNew = false;
            }
        }
        if (cellIsNew) {
            try {
                cellScanProduct.process();
            } catch (Exception e) {
                logger.error("CELL product processing did not complete.", e);
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
        for (Entry<String, Pattern> entry : patternKeys.entrySet()) {
            if (entry.getValue().pattern().equals(pattern.pattern())) {
                return entry.getKey();
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
     * Determines if a DMD record needs to be closed
     *
     * @return
     */
    public boolean dmdIsNew() {
        return dmdIsNew;
    }

    /**
     * Determines if a TVS record needs to be closed
     *
     * @return
     */
    public boolean tvsIsNew() {
        return tvsIsNew;
    }

    /**
     * Determines if an MD record needs to be closed
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
            logger.debug("Grid record data unavailable.", e);
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
            logger.debug("RADAR record data unavailable.", e);
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
            logger.debug("No UA record data available.", e);
        }

        return sounding;
    }

    /**
     * Set Grid Record in Model Data
     * 
     * @param prodType
     * @param guri
     */
    public void setGridRecord(String prodType, String guri) {
        String modelName = site.getModelParameter(prodType).getModelName();

        try {
            scan.getCache().getModelData().setGridRecord(modelName, prodType,
                    getRecordGrib(guri));
        } catch (Exception e) {
            logger.debug("Grid record setter failed.", e);
        }
    }

    /**
     * Set Grib Record in Model Data
     * 
     * @param prodType
     * @param rec
     */
    public void setGridRecord(String prodType, GridRecord rec) {
        String modelName = site.getModelParameter(prodType).getModelName();

        try {
            scan.getCache().getModelData().setGridRecord(modelName, prodType,
                    rec);
        } catch (Exception e) {
            logger.debug("Grid record setter failed.", e);
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
    public void initializeGridRecords() {
        try {
            ScanGridRecordSet records = getGridData();

            if (records.getCape() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(CAPEProduct.cape).getModelName(),
                        CAPEProduct.cape, records.getCape());
            }

            if (records.getHeli() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(HELIProduct.heli).getModelName(),
                        HELIProduct.heli, records.getHeli());
            }

            if (records.getU500() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(U500Product.U500).getModelName(),
                        U500Product.U500, records.getU500());
            }

            if (records.getU700() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(U700Product.U700).getModelName(),
                        U700Product.U700, records.getU700());
            }

            if (records.getV700() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(V700Product.V700).getModelName(),
                        V700Product.V700, records.getV700());
            }

            if (records.getGh500() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(GH500Product.GH500)
                                .getModelName(),
                        GH500Product.GH500, records.getGh500());
            }

            if (records.getGh1000() != null) {
                scan.getCache().getModelData().setGridRecord(
                        site.getModelParameter(GH1000Product.GH1000)
                                .getModelName(),
                        GH1000Product.GH1000, records.getGh1000());
            }
        } catch (Exception e) {
            logger.error("Grid record initialization failed", e);
        }
    }

    /**
     * Get Grib Record from Model Data
     *
     * @param prodType
     * @return
     */
    public GridRecord getGridRecord(String prodType) {
        GridRecord rec = null;
        String modelName = site.getModelParameter(prodType).getModelName();
        try {
            rec = scan.getCache().getModelData().getGridRecord(modelName,
                    prodType);
        } catch (Exception e) {
            logger.debug("No Grid Data available.", e);
        }
        return rec;
    }

    /**
     * Set Radar Record in Radar Data
     *
     * @param prodType
     * @param ruri
     */
    public void setRadarRecord(String prodType, String ruri) {
        try {
            scan.getCache().getRadarData(getIcao()).setRadarRecord(prodType,
                    getRecordRadar(ruri));
        } catch (Exception e) {
            logger.debug("Radar record setter failed.", e);
        }
    }

    /**
     * Get Radar Record from Radar Data
     *
     * @param prodType
     * @return
     */
    public RadarRecord getRadarRecord(String prodType) {
        RadarRecord rec = null;
        try {
            rec = scan.getCache().getRadarData(getIcao())
                    .getRadarRecord(prodType);
        } catch (Exception e) {
            logger.debug("No Radar Data available.", e);
        }
        return rec;
    }

    /**
     * Set Sounding Record in Sounding Data
     *
     * @param prodType
     * @param suri
     */
    public void setSoundingRecord(String prodType, String suri) {
        try {
            VerticalSounding[] vs = getRecordSounding(suri);
            if ((vs != null) && (vs.length > 0)) {
                getSoundingData().setSoundingRecord(prodType,
                        vs[vs.length - 1]);
                getSoundingData().setEnvironmentalData(vs[vs.length - 1]);
            }
        } catch (Exception e) {
            logger.error("UA record setter failed.", e);
        }
    }

    /**
     * Get Sounding Record from Sounding Data
     *
     * @param prodType
     * @return
     */
    public VerticalSounding getSoundingRecord(String prodType) {
        VerticalSounding rec = null;
        try {
            rec = getSoundingData().getSoundingRecord(prodType);
        } catch (Exception e) {
            logger.debug("No UA Data available.", e);
        }
        return rec;
    }

    /**
     * Get lightning data
     *
     * @param date
     * @return
     */
    public List<BinLightningRecord> getLightningData(Date date) {
        return scan.getCache().getLigtningData().getLightningRecords(date);
    }

    /**
     * checks for data
     *
     * @param date
     * @return
     */
    public boolean isLightning(Date date) {
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
        initializeGridRecords();
        setSoundingRecord(UAProduct.UA, "");
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

        private ScanProduct scanProduct = null;

        @Override
        public void run() {
            logger.debug(
                    "ProcessProduct: Starting thread " + scanProduct.getUri());
            process();
            logger.debug(
                    "ProcessProduct: Finishing thread " + scanProduct.getUri());
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
                logger.error("ProcessProduct: did not complete.", e);
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
            IPathManager pm = PathManagerFactory.getPathManager();
            String path = pm.getStaticFile(getFullPathAndFileName())
                    .getAbsolutePath();

            dataXML = jaxb.unmarshalFromXmlFile(path);
        } catch (Exception e) {
            logger.error("Scan Alarm data not available", e);
        }

        return dataXML;
    }

    /**
     * Get the grid data. Uses the environmental data backup for the "pull
     * strategy" for All model params.
     */
    protected ScanGridRecordSet getGridData() throws Exception {
        SCANSiteXML site = scan.getRunConfig().getSiteConfig(getIcao());
        logger.info(" site = " + site.getScanSite());
        int interval = TimeUtil.MINUTES_PER_DAY;
        ScanGridRecordSet result = new ScanGridRecordSet();

        String modelCape = site.getModelParameter(CAPEProduct.cape)
                .getModelName();
        GridRecord cape = CAPEProduct.getGridRecord(interval, modelCape);
        logger.info("modelCape = " + modelCape + "; record = " + cape);
        DATUtils.populateGridRecord(cape);
        result.setCape(cape);

        String modelHeli = site.getModelParameter(HELIProduct.heli)
                .getModelName();
        GridRecord heli = HELIProduct.getGridRecord(interval, modelHeli);
        logger.info("modelHeli = " + modelHeli + "; record = " + heli);
        DATUtils.populateGridRecord(heli);
        result.setHeli(heli);

        String modelU500 = site.getModelParameter(U500Product.U500)
                .getModelName();
        GridRecord u500 = U500Product.getGridRecord(interval, modelU500);
        logger.info("modelU500 = " + modelU500 + "; record = " + u500);
        DATUtils.populateGridRecord(u500);
        result.setU500(u500);

        String modelU700 = site.getModelParameter(U700Product.U700)
                .getModelName();
        GridRecord u700 = U700Product.getGridRecord(interval, modelU700);
        logger.info("modelU700 = " + modelU700 + "; record = " + u700);
        DATUtils.populateGridRecord(u700);
        result.setU700(u700);

        String modelV700 = site.getModelParameter(V700Product.V700)
                .getModelName();
        GridRecord v700 = V700Product.getGridRecord(interval, modelV700);
        logger.info("modelV700 = " + modelV700 + "; record = " + v700);
        DATUtils.populateGridRecord(v700);
        result.setV700(v700);

        String modelGH500 = site.getModelParameter(GH500Product.GH500)
                .getModelName();
        GridRecord gh500 = GH500Product.getGridRecord(interval, modelGH500);
        logger.info("modelGH500 = " + modelGH500 + "; record = " + gh500);
        DATUtils.populateGridRecord(gh500);
        result.setGh500(gh500);

        String modelGH1000 = site.getModelParameter(GH1000Product.GH1000)
                .getModelName();
        GridRecord gh1000 = GH1000Product.getGridRecord(interval, modelGH1000);
        logger.info("modelGH1000 = " + modelGH1000 + "; record = " + gh1000);
        DATUtils.populateGridRecord(gh1000);
        result.setGh1000(gh1000);

        return result;
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

    public Date getValidTime(ScanTables type) {
        if (type == ScanTables.CELL) {
            return cellValidTime;
        } else if (type == ScanTables.DMD) {
            return dmdValidTime;
        } else if (type == ScanTables.TVS) {
            return tvsValidTime;
        } else if (type == ScanTables.MESO) {
            return mdValidTime;
        }
        return null;
    }

    public void setCellValidTime(Date cellValidTime) {
        this.cellValidTime = cellValidTime;
    }

    public void setDmdValidTime(Date dmdValidTime) {
        this.dmdValidTime = dmdValidTime;
    }

    public void setTvsValidTime(Date tvsValidTime) {
        this.tvsValidTime = tvsValidTime;
    }

    public void setMdValidTime(Date mdValidTime) {
        this.mdValidTime = mdValidTime;
    }

}
