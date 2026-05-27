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
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFTIException;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;
import com.raytheon.uf.common.monitor.xml.FFTISourceXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * FFTIProcessor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2011            dhladky     Initial creation
 * Jul 11, 2012            dhladky     Edited for FFTI work
 * Feb 01, 2013 1569       D. Hladky   Added constants, records writing switched
 *                                     to pypies
 * Apr 16, 2013 1912       bsteffen    Initial bulk hdf5 access for ffmp
 * Apr 18, 2013 1919       dhladky     Fixed VGB breakage
 * Jun 21, 2013 2131       bsteffen    Revert the slow part of 1919.
 * July 3, 2013 2131       dhladky     Fixed problems caused by revert.
 * Jul 15, 2013 2184       dhladky     Remove all HUC's for storage except ALL
 * Jun 14, 2018 6560       njensen     Replaced dependency on FFMPGenerator with
 *                                     FFTIContainer
 * Aug 14, 2018 6720       njensen     Use simplified enums
 * 
 * </pre>
 * 
 * @author dhladky
 */
public class FFTIProcessor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTIProcessor.class);

    /** Pattern for dates in radar */
    private static final String datePattern = "yyyy-MM-dd HH:mm:ss";

    private FFTISourceXML fftiSource = null;

    private FFTIContainer fftiContainer = null;

    public FFTIProcessor(FFTISourceXML fftiSource,
            FFTIContainer fftiContainer) {
        this.fftiSource = fftiSource;
        this.fftiContainer = fftiContainer;
    }

    /**
     * Process FFTI for this source
     * 
     * @throws FFTIException
     */
    public void processFFTI(String iSiteKey, String iDataKey)
            throws FFTIException {
        List<String> dispNameList = fftiSource.getDisplayNameList();

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

            if (source != null) {
                /*
                 * FFTI is still using display names so we will use it here too.
                 * It also then matches the names shown on the FFTI GUI.
                 */
                String displayName = source.getDisplayName();
                if (source.isGuidance()) {
                    sourceString = displayName;

                    // Mark all GUIDANCE related sources as dirty for FFTI
                    for (String fftiName : fftiContainer.getFFTINames()) {
                        if (fftiName.startsWith(sourceString)) {
                            fftiContainer.getFFTIData(fftiName).setReset(true);
                        }
                    }
                } else {
                    sourceString = displayName + "-" + iSiteKey + "-"
                            + iDataKey;

                    // Mark this source as dirty for FFTI
                    if (fftiContainer.fftiExists(sourceString)) {
                        fftiContainer.getFFTIData(sourceString).setReset(true);

                        // Mark associated sources as dirty for FFTI
                        for (String fftiName : fftiContainer.getFFTINames()) {
                            String[] name = fftiName.split("-");
                            if (name.length == 3) {
                                if (name[1].equals(displayName)
                                        && name[2].equals(iDataKey)) {
                                    fftiContainer.getFFTIData(fftiName)
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
            Date startDate, Date endDate, String wfo, SourceXML source,
            String siteKey) {
        List<String> uris = getUris(startDate, endDate, wfo, source, siteKey);

        for (String uri : uris) {
            FFMPRecord rec = new FFMPRecord(uri);

            boolean contains = false;
            if (source.isGuidance()) {
                contains = sourceContainer.containsKey(source.getSourceName());
            } else {
                contains = sourceContainer
                        .containsKey(rec.getDataTime().getRefTime());
            }

            if (!contains) {
                try {
                    rec = populateRecord(rec, template);
                    FFMPBasinData newData = rec.getBasinData();
                    sourceContainer.addFFMPEntry(rec.getDataTime().getRefTime(),
                            source, newData);
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Source: " + source.getSourceName() + "  domain: "
                                    + wfo
                                    + " : failed to retrieve FFMP/FFTI Data ",
                            e);
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
     * @param siteKey
     * @return
     */
    private static List<String> getUris(Date startDate, Date endDate,
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

        List<String> uris = new ArrayList<>();

        try {
            CoreDao dao = new CoreDao(DaoConfig.forDatabase(FFMPUtils.META_DB));
            Object[] results = dao.executeSQLQuery(query.toString());

            if (results.length > 0) {
                for (Object result : results) {
                    if (result != null) {
                        uris.add((String) result);
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Source: " + source.getSourceName() + " domain: " + wfo
                            + " : failed to query",
                    e);
        }

        return uris;
    }

    /**
     * Get populated FFMPRecord
     * 
     * @param rec
     * @param template
     * @return
     * @throws PluginException
     */
    private static FFMPRecord populateRecord(FFMPRecord rec,
            FFMPTemplates template) throws PluginException {
        try {
            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(rec.getSourceName());

            // check for gage(VGB) types, if so process as a VGB
            if (source.getSourceType() == SourceType.GAGE) {
                rec.retrieveVirtualMapFromDataStore(template);
            } else {
                rec.retrieveMapFromDataStore(template);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Source: " + rec.getSourceName() + " sitekey: "
                            + " domain: " + rec.getWfo()
                            + " : failed to populate records in FFMP/FFTI",
                    e);
        }

        return rec;
    }
}
