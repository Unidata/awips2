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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFTIDataManager;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.dat.utils.DATUtils;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import com.raytheon.uf.edex.plugin.ffmp.FFMPURIGenerateMessage;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * FFMPConfig object
 * 
 * Hold config for FFMPGenerator/ FFMPProcessor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/30/2009   2521       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPConfig {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPConfig.class);

    /** Our generator reference */
    private FFMPGenerator ffmpgen = null;

    /** The generate message */
    private FFMPURIGenerateMessage genMessage = null;

    /** the latitude/longitude */
    private Coordinate latlon = null;

    /** hash of source to datakey mapping **/
    private HashMap<String, String> dataKeys = null;

    /** the cwa, three letter ID */
    private String cwa = null;

    /** create a date formatter for XMRG **/
    public SimpleDateFormat xmrgDateFmt = new SimpleDateFormat("yyyyMMddHHmm");

    /** create a date formatter for SQL **/
    public SimpleDateFormat sqlDateFmt = new SimpleDateFormat(
            "MMM dd yy HH:mm:ss");

    /** FFMP record reftime as string **/
    private String dateString = null;

    /** FFMP record reftime **/
    private Date date = null;

    /** process Source > (dataKey - Object) **/
    public HashMap<String, HashMap<String, Object>> sources = null;

    /** FFTI sources **/
    public ArrayList<ProductRunXML> fftiSources = null;

    /** virtual sources **/
    public HashMap<String, Object> virtualSources = null;

    private GeometryFactory factory = null;

    /** Used for FFTI comparisons **/
    public FFTIDataManager fdm = null;

    /**
     * Constructor
     * 
     * @param genMessage
     * @param ffmpgen
     * @throws Exception
     */
    public FFMPConfig(FFMPURIGenerateMessage genMessage, FFMPGenerator ffmpgen)
            throws Exception {

        this.ffmpgen = ffmpgen;
        this.genMessage = genMessage;

        // template config
        ffmpgen.tempConfig = FFMPTemplateConfigurationManager.getInstance();
        ffmpgen.tempConfig.readConfigXml();

        fdm = FFTIDataManager.getInstance();

        // setup time matchers
        xmrgDateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        sqlDateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        setCWA(genMessage.getCwa());
        date = new Date();
        setDateString(sqlDateFmt.format(date));
        setDate(sqlDateFmt.parse(getDateString()));

        try {
            setSourceHashes();
        } catch (Exception e) {
            e.printStackTrace();
            ffmpgen.logger
                    .error("Couldn't run FFMP. Invalid Config for Sources!!!");
        }
    }

    /**
     * Pull the float array from the Grib
     * 
     * @param dataUri
     * @return
     */
    public float[] getGribData(GridRecord rec) {
        return ((FloatDataRecord) rec.getMessageData()).getFloatData();
    }

    /**
     * gets The FFMP generator
     * 
     * @return
     */
    public FFMPGenerator getGenerator() {
        return ffmpgen;
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
    public void setLatLon(Coordinate latlon) {
        this.latlon = latlon;
    }

    /**
     * Sets the dataKey mapping for a source
     * 
     * @param icao
     */
    public void addDataKey(String sourceName, String dataKey) {
        dataKeys.put(sourceName, dataKey);
    }

    /**
     * Sets the dataKey mapping for a source
     * 
     * @return
     */
    public String getDataKey(String sourceName) {
        return dataKeys.get(sourceName);
    }

    /**
     * sets the cwa
     * 
     * @param icao
     */
    public void setCWA(String cwa) {
        this.cwa = cwa;
    }

    /**
     * gets the cwa for this FFMP
     * 
     * @return
     */
    public String getCWA() {
        return cwa;
    }

    /**
     * Get the DHR record
     * 
     * @param uri
     * @return
     */
    private Object getDHRRecord(String uri) {
        Object record = null;
        try {
            record = ScanCommonUtils.getRadarRecord(uri);
        } catch (PluginException e) {
            e.printStackTrace();
        }
        return record;
    }

    /**
     * Gets the objects for PDO types
     * 
     * @param xml
     * @param uri
     * @return
     */
    private IMonitorProcessing getPDOFile(SourceXML xml, String uri) {
        IMonitorProcessing obj = null;
        try {
            obj = (IMonitorProcessing) DATUtils.getPDORecord(uri, xml);
        } catch (PluginException e) {
            e.printStackTrace();
        }
        return obj;
    }

    /**
     * Gets the FFG record
     * 
     * @return
     */
    public GridRecord getGrib(String uri) {
        GridRecord rec = null;
        try {
            rec = DATUtils.getGridRecord(uri);
        } catch (PluginException e) {
            e.printStackTrace();
        }
        return rec;
    }

    /**
     * sets the DHR date
     * 
     * @param date
     */
    public void setDateString(String dateString) {
        this.dateString = dateString;
    }

    /**
     * returns a formatted date for DHR
     * 
     * @return
     */
    public String getDateString() {
        return dateString;
    }

    /**
     * sets the DHR date
     * 
     * @param date
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * returns a formatted date for DHR
     * 
     * @return
     */
    public Date getDate() {
        return date;
    }

    /**
     * Get the XMRG file from the directory
     * 
     * @param file
     * @return
     */
    private XmrgFile getXmrgFile(String fileName) {

        return new XmrgFile(fileName);
    }

    /**
     * Setup the sources
     */
    public void setSourceHashes() {
        try {
            sources = new HashMap<String, HashMap<String, Object>>();
            FFMPSourceConfigurationManager sourceConfig = FFMPSourceConfigurationManager
                    .getInstance();
            for (SourceXML source : sourceConfig.getSources()) {

                String sourceName = source.getSourceName();
                // hash of this sources process ready parts
                HashMap<String, Object> sourceHash = new HashMap<String, Object>();

                Map<String, String> sourceMap = genMessage.getSources();
                for (String dataKey : sourceMap.keySet()) {

                    String[] keys = dataKey.split(":");
                    String checkSourceName = keys[0];
                    String sourceKey = keys[1];

                    if (checkSourceName.equals(sourceName)) {

                        String dataUri = sourceMap.get(dataKey);

                        if (source.getDataType().equals(
                                FFMPSourceConfigurationManager.DATA_TYPE.XMRG
                                        .getDataType())) {

                            Object dataObject = getXMRGFile(dataUri);

                            if (dataObject != null) {
                                // process as a VGB too
                                ProductXML product = sourceConfig
                                        .getProduct(source.getSourceName());
                                // This is something that only HPE/N data does
                                // reset the sourceKey to the real key, not just
                                // XMRG
                                if (product != null) {

                                    for (ProductRunXML productRun : ffmpgen
                                            .getRunConfig().getRunner(getCWA())
                                            .getProducts()) {
                                        if (productRun.getProductName().equals(
                                                product.getPrimarySource())) {
                                            sourceKey = productRun
                                                    .getProductKey();
                                        }
                                    }

                                    HashMap<String, Object> virtSourceHash = new HashMap<String, Object>();
                                    virtSourceHash.put(sourceKey, dataObject);
                                    sources.put(product.getVirtual(),
                                            virtSourceHash);
                                } else {
                                    // NON-QPE sources
                                    String primarySource = ffmpgen
                                            .getSourceConfig()
                                            .getPrimarySource(source);

                                    for (ProductRunXML productRun : ffmpgen
                                            .getRunConfig().getRunner(getCWA())
                                            .getProducts()) {
                                        if (productRun.getProductName().equals(
                                                primarySource)) {
                                            sourceKey = productRun
                                                    .getProductKey();
                                        }
                                    }
                                }

                                sourceHash.put(sourceKey, dataObject);
                            }
                        } else if (source.getDataType().equals(
                                FFMPSourceConfigurationManager.DATA_TYPE.PDO
                                        .getDataType())) {
                            Object dataObject = getPDOFile(source, dataUri);
                            if (dataObject != null) {
                                sourceHash.put(sourceKey, dataObject);
                            }
                        } else if (source.getDataType().equals(
                                FFMPSourceConfigurationManager.DATA_TYPE.RADAR
                                        .getDataType())) {
                            Object dataObject = getDHRRecord(dataUri);

                            if (dataObject != null) {
                                // process as a VGB too
                                ProductXML product = sourceConfig
                                        .getProduct(source.getSourceName());
                                if (product != null) {
                                    HashMap<String, Object> virtSourceHash = new HashMap<String, Object>();
                                    virtSourceHash.put(sourceKey, dataObject);
                                    sources.put(product.getVirtual(),
                                            virtSourceHash);
                                }

                                sourceHash.put(sourceKey, dataObject);
                            }
                        }
                        // special here because we can have more than one rfc
                        // for some guidance source types
                        else if (source.getDataType().equals(
                                FFMPSourceConfigurationManager.DATA_TYPE.GRID
                                        .getDataType())) {
                            Object dataObject = getGrib(dataUri);
                            if (dataObject != null) {
                                sourceHash.put(sourceKey, dataObject);
                            }
                        }
                    }
                }

                if (sourceHash.size() > 0) {
                    sources.put(sourceName, sourceHash);
                }
            }
        } catch (Exception e) {
            sources = null;
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't create FFMP Config... Improper source configuration detected."
                            + e);
        }
    }

    /**
     * Grab the XMRG file for use
     * 
     * @param path
     * @return
     */
    public XmrgFile getXMRGFile(String filePath) {

        XmrgFile file = null;

        try {
            file = getXmrgFile(filePath);
            file.load();

        } catch (Exception e) {
            e.printStackTrace();
        }

        return file;
    }

    /**
     * Get the sources
     * 
     * @return
     */
    public HashMap<String, HashMap<String, Object>> getSources() {
        return sources;
    }

    /**
     * Geo factory
     * 
     * @return
     */
    public GeometryFactory getGeometryFactory() {
        if (factory == null) {
            factory = new GeometryFactory();
        }
        return factory;
    }

    /**
     * Get the hash of data for this source
     * 
     * @param sourceName
     * @return
     */
    public HashMap<String, Object> getSourceData(String sourceName) {
        return sources.get(sourceName);
    }

}
