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
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.dao.FFMPDao;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.FFTISourceXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;

/**
 * FFTIProcessor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2011            dhladky     Initial creation
 * July 11, 2012            dhladky    Edited for FFTI work
 * 02/01/13     1569        D. Hladky   Added constants, records writing switched to pypies
 * </pre>
 * Apr 16, 2013 1912       bsteffen    Initial bulk hdf5 access for ffmp
 * Apr 18, 2013 1919       dhladky     Fixed VGB breakage
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFTIProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTIProcessor.class);

    private FFTISourceXML fftiSource = null;

    private FFMPRecord ffmpRec = null;

    private FFMPGenerator ffmpgen = null;

    /** Pattern for dates in radar */
    public static String datePattern = "yyyy-MM-dd HH:mm:ss";

    /**
     * useful constructor
     * 
     * @param fdm
     * @param ffmpgen
     * @param ffmpRec
     * @param source
     */
    public FFTIProcessor(FFMPGenerator ffmpgen, FFMPRecord ffmpRec,
            FFTISourceXML fftiSource) {

        this.ffmpgen = ffmpgen;
        this.ffmpRec = ffmpRec;
        this.fftiSource = fftiSource;
    }

    /**
     * Process FFTI for this source
     */
    public void processFFTI() {

        ArrayList<String> dispNameList = fftiSource.getDisplayNameList();
        String iSiteKey = ffmpRec.getSiteKey();
        String iDataKey = ffmpRec.getDataKey();

        for (String sourceNameString : dispNameList) {

            String sourceString = sourceNameString;

            String[] parts = sourceNameString.split("-");
            SourceXML source = null;

            if (parts.length > 1) {
                iSiteKey = parts[0];
                sourceString = parts[1];
            }

            source = FFMPSourceConfigurationManager.getInstance()
                    .getSourceByDisplayName(sourceString);

            // System.out.println("Source XML: "+source.getDisplayName());

            if (source != null) {

                if (source.getSourceType().equals(
                        FFMPSourceConfigurationManager.SOURCE_TYPE.GUIDANCE
                                .getSourceType())) {

                    sourceString = source.getDisplayName();

                    // Mark all GUIDANCE related sources as dirty for FFTI
                    for (String fftiName : ffmpgen.getFFTIDataContainer()
                            .keySet()) {
                        if (fftiName.startsWith(sourceString)) {
                            // System.out
                            // .println("Resetting FFTI source for processing!!!!! "
                            // + fftiName);
                            ffmpgen.getFFTIData(fftiName).setReset(true);
                        }
                    }

                } else {

                    sourceString = source.getDisplayName() + "-" + iSiteKey
                            + "-" + iDataKey;

                    // Mark this source as dirty for FFTI
                    if (ffmpgen.isFFTI(sourceString)) {
                        // System.out
                        // .println("Resetting FFTI source for processing!!!!! "
                        // + sourceString);
                        ffmpgen.getFFTIData(sourceString).setReset(true);

                        // Mark associated sources as dirty for FFTI
                        for (String fftiName : ffmpgen.getFFTIDataContainer()
                                .keySet()) {
                            String[] name = fftiName.split("-");
                            if (name.length == 3) {
                                if (name[1].equals(source.getDisplayName())
                                        && name[2].equals(iDataKey)) {
                                    // System.out
                                    // .println("Resetting FFTI source for processing!!!!! "
                                    // + fftiName);
                                    ffmpgen.getFFTIData(fftiName)
                                            .setReset(true);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Populates the FFTI Data back to the determined date
     * 
     * @param sourceContainer
     * @param template
     * @param startDate
     * @param endDate
     * @param wfo
     * @param source
     * @return
     */
    public static FFMPDataContainer populateDataContainer(
            FFMPDataContainer sourceContainer, FFMPTemplates template,
            ArrayList<String> hucs, Date startDate, Date endDate, String wfo,
            SourceXML source, String siteKey) {

        ArrayList<String> uris = getUris(startDate, endDate, wfo, source,
                siteKey);
        // System.out.println("Number of Records querried: " + siteKey + " : "
        // + uris.size());

        for (String uri : uris) {

            FFMPRecord rec = new FFMPRecord(uri);

            boolean contains = false;

            if (source.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                contains = sourceContainer.containsKey(source.getSourceName());
                // System.out.println("Processing FFG source!!!!!"
                // + source.getSourceName());
            } else {
                contains = sourceContainer.containsKey(rec.getDataTime()
                        .getRefTime());
            }

            if (!contains) {
                try {

                    if (hucs == null) {
                        hucs = new ArrayList<String>();
                        hucs.add(FFMPRecord.ALL);
                    }

                    for (String huc : hucs) {

                        rec = populateRecord(rec, huc, template);
                        FFMPBasinData newData = rec.getBasinData(huc);
                        sourceContainer.addFFMPEntry(rec.getDataTime()
                                .getRefTime(), source, newData, huc, siteKey);

                    }

                    // System.out.println("Adding Time: "
                    // + rec.getDataTime().getRefTime());

                } catch (PluginException e) {
                    e.printStackTrace();
                    statusHandler.handle(Priority.ERROR,
                            "Source: " + source.getDisplayName() + "  domain: "
                                    + wfo
                                    + " : failed to retrieve FFMP/FFTI Data ");
                }
            }
        }

        return sourceContainer;
    }

    /**
     * Get the uris for this FFTI source
     * 
     * @param startDate
     * @param endDate
     * @param wfo
     * @param source
     * @return
     */
    public static ArrayList<String> getUris(Date startDate, Date endDate,
            String wfo, SourceXML source, String siteKey) {

        SimpleDateFormat datef = new SimpleDateFormat(datePattern);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        StringBuilder query = new StringBuilder(200);

        query.append("select datauri from ffmp where wfo = '");
        query.append(wfo);
        query.append("' and sourcename = '");
        query.append(source.getSourceName());
        query.append("' and sitekey = '");
        query.append(siteKey);
        if (!source.isMosaic()) {
            query.append("' and datakey = '");
            query.append(siteKey);
        }
        query.append("' and reftime >= '");
        query.append(datef.format(startDate));
        query.append("' and reftime < '");
        query.append(datef.format(endDate));

        query.append("' order by reftime desc");
        // System.out.println("URI query: " + query.toString());

        ArrayList<String> uris = new ArrayList<String>();

        try {
            CoreDao dao = new CoreDao(DaoConfig.forDatabase(FFMPUtils.META_DB));
            Object[] results = dao.executeSQLQuery(query.toString());

            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    Object result = results[i];
                    if (result != null) {
                        /*
                         * System.out.println("Adding URI to FFTI list: " +
                         * (String) result);
                         */
                        uris.add((String) result);
                    }
                }
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Source: " + source.getSourceName() + " domain: " + wfo
                            + " : failed to query");
        }

        return uris;
    }

    /**
     * Get "All" basin container populated FFMPRecord
     * 
     * @param rec
     * @param template
     * @return
     * @throws PluginException
     */
    public static FFMPRecord populateRecord(FFMPRecord rec, String huc,
            FFMPTemplates template) throws PluginException {

        try {

            FFMPDao dao = (FFMPDao) PluginFactory.getInstance().getPluginDao(
                    rec.getPluginName());
            rec = (FFMPRecord) dao.getMetadata(rec.getDataURI());
            
            if (rec.getPluginName() == null) {
                //return rec;
                rec.setPluginName("ffmp");
            }

            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(rec.getSourceName());
            
            // check for gage(VGB) types, if so process as a VGB
            if (source.getSourceType().equals(SOURCE_TYPE.GAGE.getSourceType())) {
                rec.retrieveVirtualMapFromDataStore(template, huc);
            } else {
                rec.retrieveMapFromDataStore(template, huc);
            }

            // System.out.println("Size of huc: "
            // + rec.getBasinData(huc).getBasins().size());
        } catch (Exception se) {

            statusHandler.handle(Priority.ERROR,
                    "Source: " + rec.getSourceName() + " sitekey: "
                            + " domain: " + rec.getWfo()
                            + " : failed to populate records in FFMP/FFTI");
        }

        return rec;
    }
}
