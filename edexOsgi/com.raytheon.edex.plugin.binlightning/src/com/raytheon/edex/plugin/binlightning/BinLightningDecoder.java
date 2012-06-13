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
package com.raytheon.edex.plugin.binlightning;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.binlightning.impl.BinLightningFactory;
import com.raytheon.edex.plugin.binlightning.impl.IBinLightningDecoder;
import com.raytheon.edex.plugin.binlightning.impl.LightningDataSource;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IBinDataSource;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * AWIPS decoder adapter strategy for binary lightning data.<br/>
 * 
 * Normal usage for this adapter is<br/>
 * <code>
 *   BinLightningDecoder dec = new BinLightningDecoder();
 *   dec.setMessage(data);
 *   while(dec.hasNext())
 *   {
 *      BinLightningRecord r = dec.decode();
 *      // do something with record.
 *   }
 *   dec.dispose();
 * </code>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20070817            379 jkorman     Changed log info to debug in decode().
 * 20070821            379 jkorman     Added SFPA41 lightning data pattern.
 * 20070912            379 jkorman     Code review cleanup.
 * 20070920            379 jkorman     Check for null persistence time.
 * 20070924            379 jkorman     Removed HDFGroup code. Set insert_time
 *                                     directly in decode.
 * 20070926            379 jkorman     Updated to set DataTime.
 * 20080318           1026 jkorman     Added debug strike info.
 * 20080408           1039 jkorman     Added traceId for tracing data. 
 * 11/11/08           1684 chammack    Refactored for camel integration
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BinLightningDecoder extends AbstractDecoder {
    private static final String SFUS_PATTERN = "SFUS41 KWBC \\d{6}[^\\r\\n]*[\\r\\n]+";

    private static final String SFPA_PATTERN = "SFPA41 KWBC \\d{6}[^\\r\\n]*[\\r\\n]+";

    // Allow ingest up to 10 minutes into the future.
    private static final long TEN_MINUTES = 10 * 60 * 1000L;

    private SimpleDateFormat SDF;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Default lightning strike type for FLASH messages. RT_FLASH documents
     * indicate no default, but D2D code defaults to STRIKE_CG also.
     */
    public LtgStrikeType DEFAULT_FLASH_TYPE = LtgStrikeType.STRIKE_CG;

    private String traceId = null;

    /**
     * Construct a BinLightning decoder. Calling hasNext() after construction
     * will return false, decode() will return a null.
     */
    public BinLightningDecoder() {
        SDF = new SimpleDateFormat("yyyyMMddHHmmss");
        SDF.setTimeZone(TimeZone.getTimeZone("Zulu"));
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        String traceId = null;
        PluginDataObject[] reports = new PluginDataObject[0];
        if (data != null) {

            traceId = (String) headers.get(DecoderTools.INGEST_FILE_NAME);

            WMOHeader wmoHdr = new WMOHeader(data);
            if (wmoHdr.isValid()) {

                Calendar baseTime = TimeTools.findDataTime(wmoHdr.getYYGGgg(),
                        headers);

                byte[] pdata = DecoderTools.stripWMOHeader(data, SFUS_PATTERN);
                if (pdata == null) {
                    pdata = DecoderTools.stripWMOHeader(data, SFPA_PATTERN);
                }
                if (pdata != null) {

                    // Init all values before this decode cycle. This resets all
                    // internal
                    // decoder state.
                    ArrayList<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();

                    if ((pdata == null) || (pdata.length == 0)) {
                        return new PluginDataObject[0];
                    }

                    IBinDataSource msgData = new LightningDataSource(pdata);

                    boolean continueDecode = true;
                    while (continueDecode) {
                        IBinLightningDecoder decoder = BinLightningFactory
                                .getDecoder(msgData);

                        switch (decoder.getError()) {
                        case IBinLightningDecoder.NO_ERROR: {
                            for (LightningStrikePoint strike : decoder) {
                                strikes.add(strike);
                            }
                            break;
                        }
                        default: {
                            continueDecode = false;
                        }
                        }
                    }

                    BinLightningRecord report = null;

                    if (strikes.size() > 0) {
                        report = new BinLightningRecord(strikes.size());
                        for (LightningStrikePoint strike : strikes) {
                            report.addStrike(strike);
                            logger.debug(traceId + "-" + strike);
                        }
                    } else {
                        return new PluginDataObject[0];
                    }

                    Calendar c = TimeTools.copy(baseTime);
                    if (c == null) {
                        throw new DecoderException(traceId
                                + "-Error decoding times");
                    }

                    Calendar cStart = report.getStartTime();
                    if (cStart.getTimeInMillis() > c.getTimeInMillis()
                            + TEN_MINUTES) {
                        synchronized (SDF) {
                            logger.info("Discarding future data for " + traceId
                                    + " at " + SDF.format(cStart.getTime()));
                        }
                    } else {
                        Calendar cStop = report.getStopTime();

                        TimeRange range = new TimeRange(
                                cStart.getTimeInMillis(),
                                cStop.getTimeInMillis());

                        DataTime dataTime = new DataTime(cStart, range);
                        report.setDataTime(dataTime);

                        if (report != null) {
                            report.setTraceId(traceId);
                            report.setPluginName("binlightning");
                            try {
                                report.constructDataURI();
                                reports = new PluginDataObject[] { report };
                            } catch (PluginException e) {
                                logger.error("Error constructing datauri", e);
                            }
                        }
                    }
                }
            } else {
                logger.error("No WMOHeader found in data");
            }
        }
        return reports;
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

}
