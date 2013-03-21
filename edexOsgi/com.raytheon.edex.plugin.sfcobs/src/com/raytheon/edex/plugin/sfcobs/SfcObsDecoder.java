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
package com.raytheon.edex.plugin.sfcobs;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.ISfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.SfcObsDecoderFactory;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder strategy for text surface observation data. Most common usage is as
 * follows. <code>
 *   SfcObsDecoder dec = new SfcObsDecoder();
 *   dec.setMessage();
 *   while(dec.hasNext())
 *   {
 *      PluginDataObject r = dec.decode();
 *      // do something with record.
 *   }
 * </code>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925     391        jkorman     Initial Coding.
 * 20071107     391        jkorman     Modified findDuplicate to use a different
 *                                     dataURI query.
 * Dec 17, 2007 600        bphillip    Added dao pool usage
 * 20080123     758        jkorman     Added code to remove observation with a 
 *                                     time in the future.
 * 20080215     887        jkorman     Added null checks in decode.
 * 20080218     887        jkorman     Reverse null checks in findDuplicate.
 * Mar 19, 2013 1785       bgonzale    Added performance status handler and added status
 *                                     to decode.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SfcObsDecoder extends AbstractDecoder {
    // Allowable future time in milliseconds.
    // private static final long ALLOWABLE_TIME = 15 * 60 * 1000;
    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "sfcobs";

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("SfcObs:");

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private boolean removeNILs = true;

    protected String traceId = "";

    public static class SfcObsDecoderInput {
        public WMOHeader wmoHeader;

        public String report;
    }

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public SfcObsDecoder() {

    }

    /**
     * Determine the removeNILs status.
     * 
     * @return Should NIL reports be removed.
     */
    public boolean isRemoveNILs() {
        return removeNILs;
    }

    /**
     * Set the removeNILs status.
     * 
     * @param removeNILs
     *            Should NIL reports be removed.
     */
    public void setRemoveNILs(boolean removeNILs) {
        this.removeNILs = removeNILs;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        SfcObsSeparator separator = SfcObsSeparator.separate(data, headers);
        List<PluginDataObject> retVal = new ArrayList<PluginDataObject>();
        HashMap<String, Boolean> obsMap = new HashMap<String, Boolean>();
        ITimer timer = TimeUtil.getTimer();

        timer.start();
        while (separator.hasNext()) {
            SfcObsDecoderInput input = separator.next();
            PluginDataObject report = null;

            logger.debug("SfcObsDecoder.decode()");

            ISfcObsDecoder decoderStrategy = SfcObsDecoderFactory
                    .getDecoderInstance(input.wmoHeader, input.report);

            if (decoderStrategy != null) {
                if (removeNILs && decoderStrategy.isNILObs()) {
                    logger.debug("Skipping NIL obs");
                } else {
                    try {
                        report = decoderStrategy.decode();
                        if (!isValidTime((ObsCommon) report, headers)) {
                            if (report != null) {
                                logger.info("Discarding future observation :"
                                        + report.getDataURI());
                            }
                            report = null;
                        }
                    } catch (Exception e) {
                        String errorText = String.format(
                                "Decode error : Message = %s",
                                decoderStrategy.getReportData());
                        logger.error(errorText, e);

                    }
                }
            }
            if (report != null) {
                report.setTraceId(traceId);
                report.setPluginName(PLUGIN_NAME);
                try {
                    report.constructDataURI();
                } catch (PluginException e) {
                    throw new DecoderException("Error Constructing dataURI", e);
                }
                if (!obsMap.containsKey(report.getDataURI())) {
                    retVal.add(report);
                    obsMap.put(report.getDataURI(), Boolean.TRUE);
                }
            }
        }
        timer.stop();
        perfLog.logDuration("Time to Decode", timer.getElapsedTime());
        return retVal.toArray(new PluginDataObject[retVal.size()]);
    }

    /**
     * Set a trace identifier for the source data.
     * 
     * @param traceId
     *            A unique identifier associated with the input data.
     */
    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

    /**
     * Checks that the observation time is not in the future.
     * 
     * @param report
     *            The ObsCommon sfcobs report to check.
     * @return Is the report time valid?
     */
    private boolean isValidTime(ObsCommon report, Headers headers) {
        boolean isValid = false;
        if (report != null) {

            Calendar curr = TimeTools.getSystemCalendar((String) headers
                    .get(DecoderTools.INGEST_FILE_NAME));

            Calendar rHour = TimeTools.copy(report.getRefHour());
            rHour.add(Calendar.MINUTE, -15);

            long delta = curr.getTimeInMillis() - rHour.getTimeInMillis();

            isValid = (delta >= 0);
        }
        return isValid;
    }

}
