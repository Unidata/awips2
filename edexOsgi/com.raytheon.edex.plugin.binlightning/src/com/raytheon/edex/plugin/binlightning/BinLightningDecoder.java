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

import gov.noaa.nws.ost.edex.plugin.binlightning.BinLightningAESKey;
import gov.noaa.nws.ost.edex.plugin.binlightning.BinLightningDataDecryptionException;
import gov.noaa.nws.ost.edex.plugin.binlightning.BinLightningDecoderUtil;
import gov.noaa.nws.ost.edex.plugin.binlightning.DecryptedLightningValidator;
import gov.noaa.nws.ost.edex.plugin.binlightning.EncryptedBinLightningCipher;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.binlightning.filter.LightningGeoFilter;
import com.raytheon.edex.plugin.binlightning.impl.BinLightningFactory;
import com.raytheon.edex.plugin.binlightning.impl.IBinDataSource;
import com.raytheon.edex.plugin.binlightning.impl.IBinLightningDecoder;
import com.raytheon.edex.plugin.binlightning.impl.LightningDataSource;
import com.raytheon.edex.plugin.binlightning.total.LightningWMOHeader;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * AWIPS decoder adapter strategy for binary lightning data.<br/>
 * 
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
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * Jun 03, 2014 3226       bclement    removed unused WMO patterns, switched to UFStatus
 *                                      removed TimeTools usage, removed constructDataURI() call
 *                                      added decodeBinLightningData() and decodeBitShiftedBinLightningData() from BinLightningDecoderUtil
 * Jun 05, 2014 3226       bclement    LightningStikePoint refactor, added extractPData()
 * Jun 09, 2014 3226       bclement    moved data array decrypt prep to EncryptedBinLightingCipher
 * Jun 10, 2014 3226       bclement    added filter support
 * Jun 19, 2014 3226       bclement    added validator callback
 * Aug 04, 2014 3488       bclement    added checkBinRange(), rebin() and finalizeRecords()
 * Apr 07, 2016 DR18763 mgamazaychikov Switched to using LightningWMOHeader.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BinLightningDecoder extends AbstractDecoder {

    // Allow ingest up to 10 minutes into the future.
    private static final long TEN_MINUTES = 10 * 60 * 1000L;

    private final SimpleDateFormat SDF;

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(BinLightningDecoder.class);

    private static final boolean REBIN_INVALID_DATA = Boolean
            .getBoolean("rebin.invalid.binlightning");

    public static final String BINLIGHTNING_KEYSTORE_PREFIX = "binlightning";

    /**
     * Default lightning strike type for FLASH messages. RT_FLASH documents
     * indicate no default, but D2D code defaults to STRIKE_CG also.
     */
    public LtgStrikeType DEFAULT_FLASH_TYPE = LtgStrikeType.CLOUD_TO_GROUND;

    private String traceId = null;

    /**
     * callback for validating decryption results
     */
    private static DecryptedLightningValidator validator = new DecryptedLightningValidator() {
        @Override
        public boolean isValid(byte[] decryptedData) {
            return BinLightningDecoderUtil.isKeepAliveRecord(decryptedData)
                    || BinLightningDecoderUtil
                            .isLightningDataRecords(decryptedData);
            /*
             * use this if keep-alive record could be mixed with lightning
             * records
             */
             // return 
             // BinLigntningDecoderUtil.isValidMixedRecordData(decryptedData);
        }
    };
   
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

            LightningWMOHeader wmoHdr = new LightningWMOHeader(data);
            if (wmoHdr.isValid()) {
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                Calendar baseTime = WMOTimeParser.findDataTime(
                        wmoHdr.getYYGGgg(), fileName);
                
                /*
                 * Because binary nature of the encrypted data, the string
                 * created with its byte[] array may not have the same length of
                 * the byte[] array length So when DecoderTools.stripWMOHeader()
                 * assumes byte[] length == String length in its logic, it is
                 * observed that it may return a shorter byte[] than the real
                 * data array. (Looks like a bug???)
                 */
//                byte[] pdata = DecoderTools.stripWMOHeader(data, SFUS_PATTERN);
//                if (pdata == null) {
//                    pdata = DecoderTools.stripWMOHeader(data, SFPA_PATTERN);
//                }
                /*
                 * instead the following is used to strip WMO header a little
                 * more safely.
                 */
                byte[] pdata = extractPData(wmoHdr, data);
    			
                if ((pdata == null) || (pdata.length == 0)) {
                    return new PluginDataObject[0];
                }
                
                /*
                 * Modified by Wufeng Zhou to handle both legacy bit-shifted and
                 * new encryted data
                 * 
                 * Preserved the legacy decoding in
                 * BinLigntningDecoderUtil.decodeBitShiftedBinLightningData(),
                 * and added logic to process both encrypted data and legacy
                 * data
                 */
                
                Collection<LightningStrikePoint> strikes = decodeBinLightningData(
                        data, pdata, traceId, wmoHdr, baseTime.getTime());

                if (strikes == null) { // keep-alive record, log and return
                	logger.info(traceId + " - found keep-alive record. ignore for now.");
                    return rval;
                }

                /*
                 * Done MOD by Wufeng Zhou
                 */
                
                // post processing data - if not keep-alive record
                BinLightningRecord report = null;
                if (strikes.size() > 0) {
                    report = LightningGeoFilter.createFilteredRecord(strikes);
                } else {
                    return new PluginDataObject[0];
                }

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
        Calendar c = TimeUtil.newCalendar(baseTime);
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
            Calendar c = TimeUtil.newCalendar(strike.getTime());
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
            BinLightningRecord record = new BinLightningRecord(bin);
            rval.add(record);
        }

        return rval;
    }

    /**
     * Remove WMO header from data and return the remaining pdata
     * 
     * @param wmoHdr
     * @param data
     * @return null if data is invalid
     */
    public static byte[] extractPData(LightningWMOHeader wmoHdr, byte[] data) {
        byte[] pdata = null;
        if (wmoHdr.isValid() && wmoHdr.getMessageDataStart() > 0) {
            pdata = new byte[data.length - wmoHdr.getMessageDataStart()];
            System.arraycopy(data, wmoHdr.getMessageDataStart(), pdata, 0,
                    data.length - wmoHdr.getMessageDataStart());
        }
        return pdata;
    }

    /**
     * Decode bin lightning data, able to handle both legacy bit-shifted and new
     * encryted data
     * 
     * The BinLightningDecoder.decode() method will use this method to decode
     * data, which will try to decrypt first, and decode the old fashioned way
     * when decryption fails
     * 
     * @author Wufeng Zhou
     * 
     * @param data
     *            - data content from file, including WMO header section
     * @param pdata
     *            - data with WMO header stripped, optional, if null, will strip
     *            WMO header internally from passed in data parameter
     * @param traceId
     *            - the file name of the data to be deoced
     * @param wmoHdr
     *            - WMOHeader, added 12/24/2013 to help distinguish bit-shifted
     *            NLDN and GLD360 data (GLD data will have header starts like
     *            SFPA)
     * @param dataDate
     *            - date of the data, optional, used as a hint to find
     *            appropriate encryption key faster
     * @return null if keep-alive record, otherwise a list (could be empty) of
     *         LightningStrikePoint
     */
    public static List<LightningStrikePoint> decodeBinLightningData(
            byte[] data, byte[] pdata, String traceId, LightningWMOHeader wmoHdr,
            Date dataDate) {
        if (pdata == null) { // if data without header not passed, we'll strip
                             // the WMO header here
            LightningWMOHeader header = new LightningWMOHeader(data);
            if (header.isValid() && header.getMessageDataStart() > 0) {
                pdata = new byte[data.length - header.getMessageDataStart()];
                System.arraycopy(data, header.getMessageDataStart(), pdata, 0,
                        data.length - header.getMessageDataStart());
            }
        }

        List<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();
        boolean needDecrypt = true; // set as default unless clear evidence says
                                    // otherwise
        boolean decodeDone = false;
        EncryptedBinLightningCipher cipher = new EncryptedBinLightningCipher();

        /*
         * Using different WMO headers to indicate whether the data is encrypted
         * or not would be a nice option.
         * However, that idea has been discussed but not adopted.
         * If in the future, WMO header can be different for legacy and
         * encrypted data, or some other metadata can be used to decide
         * whether deceyption is needed, logic can be added here.
         * 
         * Before that happens, we'll use hints and trial & error to decode the
         * data
         * Hints: Per lightning data format spec, there are 3 bytes in the WMO
         * header starting line that indicates the size of the encrypted block
         * or the ASCII sequence # for legacy bit-shifted data
         * However, the starting line is optional and AWIPS decode may not see
         * it at all because TG will strip that starting line away
         * We'll try to use this hint first, if is is not found, then trial and
         * error way to decrypt and decode
         * 
         * As of 11/05/2013, There is change in data spec. that the 3-bytes will
         * not be encoded as encrypted block size anymore (it will always be
         * transmission sequence # if present)
         * So there should have some minor changes in the logic below for
         * decoding the data.
         * However, as reading into the
         * com.raytheon.edex.plugin.binlightning.impl.BinLightningFactory.getDecoder
         * ()
         * and follow-on code, we see the following data patterns
         * for legacy bit-shifted data, which could be used to reduce guess-work
         * in data decryption:
         * The bit-shifted data will have multiple groups of the following
         * patterns:
         * 1-byte (unsigned byte): for size count
         * 1-byte (unsigned byte): for flash type:
         * 0x96 for FLASH_RPT (message size is 6 bytes each)
         * 0x97 for RT_FLASH_RPT (message size is 8 bytes each)
         * 0xd0 for OTHER_RPT (The D2D decoders declare but do not define this
         * message, so unimplemented decoder)
         * 0xd1 for COMM_RPT (The D2D decoders declare but do not define this
         * message, so unimplemented decoder)
         * 4-bytes: date time
         * multiple of 6 or 8 bytes (as determined by 2nd byte flash type) with
         * count indicated in 1st byte
         * 
         * So this is be used to determine whether the data need to be
         * decrypted.
         */

        /*
         * // looks like previous assumption on block size bytes are not valid
         * any more. 11/20/2013 if (data != null) { byte[] sizeSeqBytes =
         * BinLigntningDecoderUtil.findSizeOrSeqBytesFromWMOHeader(data); if
         * (sizeSeqBytes != null) { // if this is in the header (which may not),
         * use that as a hint to determine which decoding route to go if
         * (BinLigntningDecoderUtil
         * .isPossibleWMOHeaderSequenceNumber(sizeSeqBytes) &&
         * BinLigntningDecoderUtil
         * .getEncryptedBlockSizeFromWMOHeader(sizeSeqBytes) != pdata.length) {
         * // looks like a sequence #, and if treat as size, it does not equal
         * to the data block size, so most likely legacy data needDecrypt =
         * false; } } }
         */

        if (needDecrypt) {
            try {
                byte[] encryptedData = EncryptedBinLightningCipher
                        .prepDataForDecryption(pdata, traceId);

                byte[] decryptedData = cipher.decryptData(encryptedData,
                        dataDate, BINLIGHTNING_KEYSTORE_PREFIX, validator);
                // decrypt ok, then decode, first check if keep-alive record
                if (BinLightningDecoderUtil.isKeepAliveRecord(decryptedData)) {
                    logger.info(traceId
                            + " - Keep-alive record detected, ignore for now.");
                    decodeDone = true;
                    return null;
                }
                /*
                 * not keep-alive record, then check data validity and decode
                 * into an ArrayList<LightningStrikePoint> of strikes
                 */
                if (BinLightningDecoderUtil
                        .isLightningDataRecords(decryptedData)) {
                    strikes = BinLightningDecoderUtil
                            .decodeDecryptedBinLightningData(decryptedData);
                    decodeDone = true;
                } else {
                    logger.info(traceId
                            + " - Failed data validity check of the decrypted data, will try decode the old-fashioned way.");
                    decodeDone = false;
                }
            } catch (IllegalBlockSizeException e) {
                logger.info(traceId
                        + " - "
                        + e.getMessage()
                        + ": Decryption failed, will try decode the old-fashioned way.");
                decodeDone = false;
            } catch (BadPaddingException e) {
                logger.info(traceId
                        + " - "
                        + e.getMessage()
                        + ": Decryption failed, will try decode the old-fashioned way.");
                decodeDone = false;
            } catch (BinLightningDataDecryptionException e) {
                logger.info(traceId
                        + " - "
                        + e.getMessage()
                        + ": Decryption failed, will try decode the old-fashioned way.");
                decodeDone = false;
            }
        }

        if (decodeDone == false) { // not decoded through decrypt->decode
                                   // process, try the legacy decoder
            logger.info(traceId + " - decoding as bit-shifted data");
            /*
             * bit-shifting data format check call here will get us some more
             * information on the data, also can compare the strikes with the
             * decoder result
             */
            int estimatedStrikes = BinLightningDecoderUtil
                    .getBitShiftedDataStrikeCount(pdata);
            strikes = decodeBitShiftedBinLightningData(pdata, wmoHdr);
            if (estimatedStrikes != strikes.size()) {
                logger.warn(traceId
                        + ": bit-shifted decoder found "
                        + strikes
                        + " strikes, which is different from estimate from data pattern examination: "
                        + estimatedStrikes);
            }
        }

        return strikes;
    }

    /**
     * extracted from the original {@link #decode(byte[], Headers)} method then
     * modified by Wufeng Zhou
     * 
     * @param pdata
     * @param wmoHdr
     *            - WMOHeader, added 12/24/2013 to help distinguish bit-shifted
     *            NLDN and GLD360 data (GLD data will have header starts like
     *            SFPA)
     * @return
     */
    public static List<LightningStrikePoint> decodeBitShiftedBinLightningData(
            byte[] pdata, LightningWMOHeader wmoHdr) {
        List<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();

        IBinDataSource msgData = new LightningDataSource(pdata);

        boolean continueDecode = true;
        while (continueDecode) {
            IBinLightningDecoder decoder = BinLightningFactory
                    .getDecoder(msgData);

            switch (decoder.getError()) {
            case IBinLightningDecoder.NO_ERROR: {
                for (LightningStrikePoint strike : decoder) {
                    /*
                     * use WMO Header to distinguish NLDN or GLD360 data because
                     * no bit-shifted data spec available for GLD360.
                     * 12/24/2013, WZ
                     * The WMO header start string is defined in
                     * BinLightningAESKey.properties file (normally, GLD360 data
                     * will have WMO header
                     * starts with SFPA41, or SFPA99 for test data.)
                     */
                    String gld360WMOHeaderString = BinLightningAESKey
                            .getProps().getProperty(
                                    "binlightning.gld360WMOHeaderStartString",
                                    "");
                    if (gld360WMOHeaderString.trim().equals("") == false
                            && wmoHdr.getWmoHeader().startsWith(
                                    gld360WMOHeaderString)) {
                        // GLD360 data based on the setup
                        strike.setLightSource("GLD");
                    }
                    strikes.add(strike);
                }
                break;
            }
            default: {
                continueDecode = false;
            }
            }
        }
        return strikes;
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
