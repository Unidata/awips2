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
package com.raytheon.edex.plugin.redbook;

import java.util.ArrayList;
import java.util.Date;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.redbook.dao.RedbookDao;
import com.raytheon.edex.plugin.redbook.decoder.RedbookParser;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder strategy for RedbookDecoder data.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2008 1131       jkorman     Initial implementation.
 * May 29, 2008 1131       jkorman     Added new Separator constructor.
 * Nov 11, 2008 1684       chammack    Refactored to camel
 * Mar 27, 2009 2019       jkorman     Added code to check for non-redbook data.
 * May 24, 2012 647        dgilling    Update persistence time in
 *                                     createdBackDatedVersionIfNeeded.
 * Mar 19, 2013 1785       bgonzale    Added performance status handler and
 *                                     added  status to decode.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class RedbookDecoder extends AbstractDecoder {

    private static class ForeignDetect {
        public final boolean isForeign;

        public final String dataType;

        public ForeignDetect(String dataType, boolean isForeign) {
            this.isForeign = isForeign;
            this.dataType = dataType;
        }
    }

    private static final char SIG_CR = 0x0D;

    private static final char SIG_LF = 0x0A;

    private static final char SIG_ESC = 0x1A;

    private static final String PNG_SIG = "PNG" + String.valueOf(SIG_CR)
            + String.valueOf(SIG_LF) + String.valueOf(SIG_ESC)
            + String.valueOf(SIG_LF);

    private static final String GIF87A_SIG = "GIF87a";

    // This sig is currently not used.
    // private static final String GIF89A_SIG = "GIF89a";

    private static final String DIFAX_SIG = "DFAX";

    // Name of the plugin controlling this decoder.
    private final String PLUGIN_NAME;

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(RedbookDecoder.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("Redbook:");

    private String traceId = null;

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public RedbookDecoder(String pluginName) {
        PLUGIN_NAME = pluginName;
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] rawMessage, Headers headers)
            throws DecoderException {

        PluginDataObject[] reports = null;
        logger.debug(traceId + "- RedbookDecoder.decode()");

        try {
            if (headers != null) {
                traceId = (String) headers.get("traceId");
            }

            WMOHeader wmoHeader = new WMOHeader(rawMessage, headers);
            if (wmoHeader.isValid()) {
                ITimer timer = TimeUtil.getTimer();
                timer.start();

                int start = wmoHeader.getMessageDataStart();

                int len = rawMessage.length - start;
                byte[] data = new byte[len];
                System.arraycopy(rawMessage, start, data, 0, len);

                RedbookRecord report = null;
                ForeignDetect foreign = detectForeign(data);
                if (foreign.isForeign) {
                    logger.info(traceId
                            + "- File is not Redbook data. Type is "
                            + foreign.dataType);
                } else {
                    report = new RedbookParser(traceId, data, wmoHeader)
                            .getDecodedRecord();
                }
                if (report != null) {
                    report.setPersistenceTime(new Date());
                    try {
                        reports = createMergedRecordList(report);
                    } catch (PluginException e) {
                        logger.error(traceId + "- Error constructing datauri",
                                e);
                    }
                }
                timer.stop();
                perfLog.logDuration("Time to Decode", timer.getElapsedTime());
            } else {
                logger.error(traceId + "- No valid WMO header found in data.");
            }
        } catch (Exception e) {
            logger.error(traceId + "- Error processing Redbook data", e);
        } finally {
            if (reports == null) {
                reports = new PluginDataObject[0];
            }
        }

        return reports;
    }

    private PluginDataObject[] createMergedRecordList(RedbookRecord report)
            throws PluginException {
        ArrayList<PluginDataObject> listResult = new ArrayList<PluginDataObject>();

        RedbookRecord newRecord = report;
        while (newRecord != null) {
            // Note that this newRecord may be modified by
            // createdBackDatedVersionIfNeeded.
            listResult.add(newRecord);

            newRecord = createdBackDatedVersionIfNeeded(newRecord);
        }
        return listResult.toArray(new PluginDataObject[listResult.size()]);
    }

    private RedbookRecord createdBackDatedVersionIfNeeded(RedbookRecord record) {
        RedbookDao dao;
        RedbookRecord existingRecord;

        try {
            dao = (RedbookDao) PluginFactory.getInstance().getPluginDao(
                    PLUGIN_NAME);
            existingRecord = (RedbookRecord) dao.getMetadata(record
                    .getDataURI());
        } catch (PluginException e) {
            logger.error(traceId + "Could not create back-dated copy of "
                    + record.getDataURI(), e);
            return null;
        }

        if (existingRecord != null) {
            try {
                existingRecord = dao.getFullRecord(existingRecord);
            } catch (Exception e) {
                logger.error(
                        traceId + "Could not retrieve existing "
                                + record.getDataURI(), e);
                return null;
            }
            RedbookRecord backDatedRecord;
            try {
                backDatedRecord = existingRecord.createBackdatedVersion();
                // this must be updated so that the insert time is updated
                // and the Wes2Bridge archiver properly finds these backdated
                // records
                backDatedRecord.setPersistenceTime(new Date());
            } catch (PluginException e) {
                logger.error(traceId + "Could not create back-dated copy of "
                        + record.getDataURI(), e);
                return null;
            }
            record.setOverwriteAllowed(true);
            dao.delete(existingRecord); // replace op does not update metadata
                                        // ?!
            logger.info("Storing new version of " + record.getDataURI());

            return backDatedRecord;
        } else {
            return null;
        }
    }

    /**
     * Check here to see if we have non-redbook data. It's hard to determine
     * that we actually have valid redbook, so we check for the signatures of
     * various graphics formats.
     * 
     * @param data
     *            The message data to check.
     * @return Is the data foreign?
     */
    private static ForeignDetect detectForeign(byte[] data) {
        ForeignDetect foreignDetect = null;

        if ((data != null) && (data.length > 32)) {
            boolean foreign = false;
            String s = new String(data, 0, 32);
            // Check for some known graphics signatures.
            String foreignType = null;

            if (s.indexOf(PNG_SIG) >= 0) {
                foreign = true;
                foreignType = "PNG";
            } else if (s.indexOf(GIF87A_SIG) >= 0) {
                foreign = true;
                foreignType = "GIF87A";
            } else if (s.indexOf(PNG_SIG) >= 0) {
                foreign = true;
                foreignType = "GIF89A";
            } else if (s.indexOf(DIFAX_SIG) >= 0) {
                foreign = true;
                foreignType = "DIFAX";
            }
            foreignDetect = new ForeignDetect(foreignType, foreign);
        }
        return foreignDetect;
    }

}
