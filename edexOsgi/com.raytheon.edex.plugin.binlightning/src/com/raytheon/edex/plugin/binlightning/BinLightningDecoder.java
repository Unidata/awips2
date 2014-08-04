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

import gov.noaa.nws.ost.edex.plugin.binlightning.BinLigntningDecoderUtil;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
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
 * Aug 10, 2007 379        jkorman     Initial Coding from prototype.
 * Aug 17, 2007 379        jkorman     Changed log info to debug in decode().
 * Aug 21, 2007 379        jkorman     Added SFPA41 lightning data pattern.
 * Sep 12, 2007 379        jkorman     Code review cleanup.
 * Sep 20, 2007 379        jkorman     Check for null persistence time.
 * Sep 24, 2007 379        jkorman     Removed HDFGroup code. Set insert_time
 *                                     directly in decode.
 * Sep 26, 2007 379        jkorman     Updated to set DataTime.
 * Mar 18, 2008 1026       jkorman     Added debug strike info.
 * Apr 08, 2008 1039       jkorman     Added traceId for tracing data.
 * Nov 11, 2008 1684       chammack    Refactored for camel integration
 * May 03, 2013 DCS 112    Wufeng Zhou Modified to be able to handle both the
 *                                     new encrypted data and legacy bit-shifted
 *                                     data
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Jan 24, 2014 DR 16774   Wufeng Zhou Modified for updated Bin-lightning data spec, 
 *                                     and to used WMO header to distinguish bit-shifted 
 *                                     GLD360 and NLDN data.
 * Aug 04, 2014 3488       bclement    added checkBinRange(), rebin() and finalizeRecords()
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

    private final SimpleDateFormat SDF;

    private final Log logger = LogFactory.getLog(getClass());

    private static final boolean REBIN_INVALID_DATA = Boolean
            .getBoolean("rebin.invalid.binlightning");

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
    public PluginDataObject[] decode(byte[] data, Headers headers) throws DecoderException {

        //String traceId = null;
        PluginDataObject[] rval = new PluginDataObject[0];

        if (data != null) {
            traceId = (String) headers.get(DecoderTools.INGEST_FILE_NAME);

            WMOHeader wmoHdr = new WMOHeader(data);
            if (wmoHdr.isValid()) {

                Calendar baseTime = TimeTools.findDataTime(wmoHdr.getYYGGgg(),
                        headers);
                
                // Because binary nature of the encrypted data, the string created with its byte[] array may not have the same length of the byte[] array length 
                // So when DecoderTools.stripWMOHeader() assumes byte[] length == String length in its logic, it is observed that it may return a shorter byte[] than
                //    the real data array.  (Looks like a bug???)
//                byte[] pdata = DecoderTools.stripWMOHeader(data, SFUS_PATTERN);
//                if (pdata == null) {
//                    pdata = DecoderTools.stripWMOHeader(data, SFPA_PATTERN);
//                }
    			// instead the following is used to strip WMO header a little more safely.
                byte[] pdata = null;
    			if (wmoHdr.isValid() && wmoHdr.getMessageDataStart() > 0) {
    				pdata = new byte[data.length - wmoHdr.getMessageDataStart()];
    				System.arraycopy(data, wmoHdr.getMessageDataStart(), pdata, 0, data.length - wmoHdr.getMessageDataStart());
    			} 
    			
                if ((pdata == null) || (pdata.length == 0)) {
                    return new PluginDataObject[0];
                }
                
                //
                // Modified by Wufeng Zhou to handle both legacy bit-shifted and new encryted data
                //
                //  Preserved the legacy decoding in BinLigntningDecoderUtil.decodeBitShiftedBinLightningData(), and added logic to process 
                //     both encrypted data and legacy data
                //  
                
                Collection<LightningStrikePoint> strikes = BinLigntningDecoderUtil
                        .decodeBinLightningData(data, pdata, traceId, wmoHdr,
                                baseTime.getTime());

                if (strikes == null) { // keep-alive record, log and return
                	logger.info(traceId + " - found keep-alive record. ignore for now.");
                    return rval;
                }

                //
                // Done MOD by Wufeng Zhou
                //
                
                // post processing data - if not keep-alive record
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

                //report.setInsertTime(c); // OB13.4 source code does not have this line anymore, WZ 05/03/2013
                Collection<BinLightningRecord> records = checkBinRange(report,
                        strikes);
                rval = finalizeRecords(records, baseTime);
            }
        } else {
        	logger.error("No WMOHeader found in data");
        }
        return rval;
    }

    /**
     * Perform final actions on each record and populate a PDO array with them.
     * Any invalid records will be omitted from the return array.
     * 
     * @param records
     * @param baseTime
     * @return
     * @throws DecoderException
     */
    private PluginDataObject[] finalizeRecords(
            Collection<BinLightningRecord> records, Calendar baseTime)
            throws DecoderException {
        Calendar c = TimeTools.copy(baseTime);
        if (c == null) {
            throw new DecoderException(traceId + " - Error decoding times");
        }
        ArrayList<BinLightningRecord> rval = new ArrayList<BinLightningRecord>(
                records.size());
        for (BinLightningRecord record : records) {
            Calendar cStart = record.getStartTime();
            if (cStart.getTimeInMillis() > (c.getTimeInMillis() + TEN_MINUTES)) {
                synchronized (SDF) {
                    logger.info("Discarding future data for " + traceId
                            + " at " + SDF.format(cStart.getTime()));
                }
            } else {
                Calendar cStop = record.getStopTime();

                TimeRange range = new TimeRange(cStart.getTimeInMillis(),
                        cStop.getTimeInMillis());

                DataTime dataTime = new DataTime(cStart, range);
                record.setDataTime(dataTime);

                if (record != null) {
                    record.setTraceId(traceId);
                    rval.add(record);
                }
            }
        }
        return rval.toArray(new PluginDataObject[rval.size()]);
    }

    /**
     * Ensure that the record has a valid bin range. If it does, it will be the
     * only record in the return value. Otherwise, {@link #REBIN_INVALID_DATA}
     * is used to determine if no records should be returned or the strikes
     * should be split into valid bin ranges uses {@link #rebin(Collection)}
     * 
     * @param record
     * @param strikes
     * @return
     */
    private Collection<BinLightningRecord> checkBinRange(
            BinLightningRecord record, Collection<LightningStrikePoint> strikes) {
        Collection<BinLightningRecord> rval = Collections.emptyList();
        Calendar cStart = record.getStartTime();
        Calendar cStop = record.getStopTime();
        long binRange = cStop.getTimeInMillis() - cStart.getTimeInMillis();
        if (binRange > TimeUtil.MILLIS_PER_DAY) {
            if (REBIN_INVALID_DATA) {
                rval = rebin(strikes);
            } else {
                String rangeStart;
                String rangeEnd;
                synchronized (SDF) {
                    rangeStart = SDF.format(cStart.getTime());
                    rangeEnd = SDF.format(cStop.getTime());
                }
                logger.error("Discarding data with invalid bin range of "
                        + rangeStart + " to " + rangeEnd);
            }
        } else {
            rval = Arrays.asList(record);
        }
        return rval;
    }

    /**
     * Split the strikes into 1 day bins and create a new record for each bin
     * 
     * @param strikes
     * @return
     */
    private Collection<BinLightningRecord> rebin(
            Collection<LightningStrikePoint> strikes) {
        Map<Long, Collection<LightningStrikePoint>> binMap = new HashMap<Long, Collection<LightningStrikePoint>>(
                1);
        for (LightningStrikePoint strike : strikes) {
            Calendar c = TimeTools.getBaseCalendar(strike.getYear(),
                    strike.getMonth(), strike.getDay());
            c.set(Calendar.HOUR_OF_DAY, 0);
            c.set(Calendar.MINUTE, 0);
            c.set(Calendar.SECOND, 0);
            c.set(Calendar.MILLISECOND, 0);
            long key = c.getTimeInMillis();
            Collection<LightningStrikePoint> bin = binMap.get(key);
            if (bin == null) {
                bin = new ArrayList<LightningStrikePoint>(strikes.size());
                binMap.put(key, bin);
            }
            bin.add(strike);
        }
        Collection<BinLightningRecord> rval = new ArrayList<BinLightningRecord>(
                binMap.size());
        for (Entry<Long, Collection<LightningStrikePoint>> e : binMap
                .entrySet()) {
            Collection<LightningStrikePoint> bin = e.getValue();
            BinLightningRecord record = new BinLightningRecord(bin.size());
            for (LightningStrikePoint strike : bin) {
                record.addStrike(strike);
            }
            rval.add(record);
        }

        return rval;
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
