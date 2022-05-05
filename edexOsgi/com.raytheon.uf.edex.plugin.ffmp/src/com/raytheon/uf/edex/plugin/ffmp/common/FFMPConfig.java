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
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DataType;
import com.raytheon.uf.common.monitor.config.FFTIDataManager;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.edex.dat.utils.DATUtils;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import com.raytheon.uf.edex.plugin.ffmp.FFMPURIGenerateMessage;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;

/**
 * FFMPConfig object
 * 
 * Hold config for FFMPGenerator/ FFMPProcessor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/30/2009   2521       dhladky     Initial Creation.
 * Apr 24, 2014  2060      njensen     Removed unnecessary catch
 * Aug 08, 2015  4722      dhladky     Simplified source map additions, config.
 * Sep.09, 2015  4756      dhladky     Further simplified configuration.
 * Mar 04, 2016  5429      dhladky     Special case for RFCFFG multi-RFC mosaics.
 * Mar 29, 2016  5491      tjensen     Special case for QPFSCAN
 * Apr 02, 2016  5491      tjensen     Fixed special case for QPFSCAN to be strict
 * Jun 15, 2018  6560      njensen     Major cleanup
 * Jul 30, 2018  6720      njensen     Update for changed method names
 * Aug 14, 2018  6720      njensen     Use simplified enums
 * Jul 12, 2019  7627      mroos       Added param of CWA list and buffer size to 
 *                                     retrieveGrid
 * 
 * </pre>
 * 
 * @author dhladky
 */

public class FFMPConfig {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPConfig.class);

    /** Our generator reference */
    private FFMPGenerator ffmpgen = null;

    /** the cwa, three letter ID */
    private String cwa = null;

    /** FFMP record reftime **/
    private Date date = null;

    /** process Source > (dataKey - Object) **/
    private Map<String, Map<String, Object>> sources = null;

    /** Used for FFTI comparisons **/
    public FFTIDataManager fdm = null;

    /**
     * Constructor
     * 
     * @param genMessage
     * @param ffmpgen
     * @param cwas
     *            The list of CWAs to be configured
     * @throws Exception
     */
    public FFMPConfig(FFMPURIGenerateMessage genMessage, FFMPGenerator ffmpgen,
            String[] cwas, FFMPTemplates templates) throws Exception {
        this.ffmpgen = ffmpgen;
        fdm = FFTIDataManager.getInstance();

        // setup time matchers
        SimpleDateFormat sqlDateFmt = new SimpleDateFormat(
                "MMM dd yy HH:mm:ss");
        sqlDateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        this.cwa = genMessage.getCwa();
        date = new Date();
        String dateString = sqlDateFmt.format(date);
        date = sqlDateFmt.parse(dateString);

        try {
            setupDataMaps(genMessage, cwas, templates);
        } catch (Exception e) {
            ffmpgen.logger
                    .error("Couldn't run FFMP. Invalid Config for Sources", e);
        }
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
     * Get the RADAR record
     * 
     * @param uri
     * @return
     */
    private RadarRecord retrieveRadar(String uri) {
        RadarRecord record = null;
        try {
            record = ScanCommonUtils.getRadarRecord(uri);
        } catch (PluginException e) {
            statusHandler.error("Error retrieving radar record " + uri, e);
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
    private IMonitorProcessing retrievePDO(SourceXML xml, String uri) {
        return (IMonitorProcessing) DATUtils.getPDORecord(uri, xml);
    }

    /**
     * Gets the FFG record
     * 
     * @param uri
     *            The URI of the GridRecord to retrieve
     * @param cwas
     *            The list of CWAs that contain the relevant data
     * @return
     * @throws TransformException
     */
    private GridRecord retrieveGrid(String uri, String[] cwas, double buffer)
            throws TransformException {
        GridRecord rec = null;
        try {
            rec = DATUtils.getGridRecord(uri, buffer, cwas);
        } catch (PluginException e) {
            statusHandler.error("Error retrieving grid record " + uri, e);
        }
        return rec;
    }

    /**
     * Returns a date for this config
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
    private XmrgFile retrieveXmrgFile(String fileName) {
        return new XmrgFile(fileName);
    }

    /**
     * Set up the sources
     * 
     * @param genMessage
     * @param cwas
     *            The list of CWAs to set up the data maps for
     */
    private void setupDataMaps(FFMPURIGenerateMessage genMessage, String[] cwas,
            FFMPTemplates template) {
        try {
            sources = new HashMap<>();
            FFMPSourceConfigurationManager sourceConfig = FFMPSourceConfigurationManager
                    .getInstance();
            double buffer = template.getMaxExtent();
            for (SourceXML source : sourceConfig.getSources()) {
                String sourceName = source.getSourceName();
                // hash of this sources process ready parts
                Map<String, Object> keyToDataMap = new HashMap<>();

                Map<String, String> sourceMap = genMessage.getSources();
                for (Entry<String, String> entry : sourceMap.entrySet()) {
                    String dataKey = entry.getKey();
                    String[] keys = dataKey.split(":");
                    String checkSourceName = null;
                    String sourceKey = null;

                    if (keys != null && keys.length > 1) {
                        checkSourceName = keys[0];
                        sourceKey = keys[1];
                    } else {
                        checkSourceName = dataKey;
                    }

                    if (checkSourceName.equals(sourceName)) {
                        String dataUri = entry.getValue();
                        Object dataObject = null;

                        DataType dataType = source.getDataType();
                        switch (dataType) {
                        case XMRG:
                            dataObject = retrieveAndLoadXMRGFile(dataUri);
                            break;
                        case PDO:
                            dataObject = retrievePDO(source, dataUri);
                            break;
                        case RADAR:
                            dataObject = retrieveRadar(dataUri);
                            break;
                        case GRID:
                            dataObject = retrieveGrid(dataUri, cwas, buffer);
                            break;
                        default:
                            throw new IllegalStateException("DataType "
                                    + dataType + " is not supported");
                        }

                        keyToDataMap = setupKeyToDataMap(keyToDataMap,
                                dataObject, source, sourceKey);
                    }
                }

                if (!keyToDataMap.isEmpty()) {
                    sources.put(sourceName, keyToDataMap);
                }
            }
        } catch (Exception e) {
            sources = null;
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't create FFMP Config... Improper source configuration detected.",
                    e);
        }
    }

    /**
     * Grab the XMRG file for use
     * 
     * @param filePath
     * @return
     */
    private XmrgFile retrieveAndLoadXMRGFile(String filePath) {
        XmrgFile file = null;
        try {
            file = retrieveXmrgFile(filePath);
            file.load();
        } catch (Exception e) {
            statusHandler.error("Error retrieving xmrg file " + filePath, e);
        }

        return file;
    }

    /**
     * Get the sources
     * 
     * @return
     */
    public Map<String, Map<String, Object>> getSources() {
        return sources;
    }

    /**
     * Get the hash of data for this source
     * 
     * @param sourceName
     * @return
     */
    public Map<String, Object> getSourceData(String sourceName) {
        return sources.get(sourceName);
    }

    /**
     * Process the sources from the URIfilter and ready them for processing.
     * 
     * @param keyToDataMap
     * @param dataObject
     * @param source
     * @param sourceKey
     * @return sourceHash
     */
    private Map<String, Object> setupKeyToDataMap(
            Map<String, Object> keyToDataMap, Object dataObject,
            SourceXML source, String sourceKey) {
        if (dataObject != null) {
            // Is this a primary source?
            ProductXML product = ffmpgen.getSourceConfig()
                    .getProductByPrimarySourceName(source.getSourceName());
            // Check for a primary source
            if (product != null) {
                /*
                 * Some primary sources derive the sourceKey from their Run
                 * Config product name.
                 */
                if (source.getDataType() == DataType.XMRG
                        || sourceKey == null) {
                    for (ProductRunXML productRun : ffmpgen.getRunConfig()
                            .getRunner(getCWA()).getProducts()) {
                        if (productRun.getProductName()
                                .equals(product.getPrimarySource())) {
                            sourceKey = productRun.getProductKey();
                            break;
                        }
                    }
                }

                // If primary, create a virtual too.
                Map<String, Object> virtSourceToDataMap = new HashMap<>();
                virtSourceToDataMap.put(sourceKey, dataObject);
                sources.put(product.getVirtual(), virtSourceToDataMap);
            } else if (source.isRfc()) {
                /*
                 * The special case of RFCFFG, must have separate URI's for each
                 * RFC mosaic piece. Use existing sourceKey that designates that
                 * mosaic piece.
                 */
            } else if ("QPFSCAN".equals(source.getSourceName())) {
                /*
                 * The special case of QPFSCAN. Use existing sourceKey that
                 * designates that mosaic piece.
                 */
            } else {
                // NON Primary sources, find the primary.
                String primarySource = ffmpgen.getSourceConfig()
                        .getPrimarySource(source);
                // Find the sourceKey to run against.
                for (ProductRunXML productRun : ffmpgen.getRunConfig()
                        .getRunner(getCWA()).getProducts()) {
                    if (productRun.getProductName().equals(primarySource)) {
                        sourceKey = productRun.getProductKey();
                        break;
                    }
                }
            }
            // Add to hash of sources to be processed.
            keyToDataMap.put(sourceKey, dataObject);
        }

        return keyToDataMap;
    }

}
