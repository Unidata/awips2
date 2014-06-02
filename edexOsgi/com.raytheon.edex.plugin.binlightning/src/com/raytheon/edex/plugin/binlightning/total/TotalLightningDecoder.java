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
package com.raytheon.edex.plugin.binlightning.total;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.binlightning.BinLightningDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.BaseLightningPoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningPulsePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgPulseType;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.numeric.UnsignedNumbers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * Decoder for Earth Networks Total Lightning data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2014 3226       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TotalLightningDecoder {

    // flash types
    public static final byte CLOUD_TO_GROUND_TYPE = 0x00;

    public static final byte CLOUD_TO_CLOUD_TYPE = 0x01;

    // pulse types
    public static final byte RETURN_STROKE_TYPE = 0x00;

    public static final byte NON_RETURN_STROKE_TYPE = 0x01;

    public static final byte KEEP_ALIVE_TYPE = 0x09;

    // conversions
    public static final double LONLAT_SCALE_FACTOR = 0.0000001;

    public static final double AMPS_PER_KILO_AMP = 1000.0;

    public static final double METERS_PER_KILOMETER = 1000.0;

    // constant metadata
    public static final String DATA_SOURCE = "ENTLN";

    private static final IUFStatusHandler log = UFStatus
            .getHandler(TotalLightningDecoder.class);

    /**
     * Parse total lightning data into BinLightningRecords
     * 
     * @param data
     * @param headers
     * @return
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {
        PluginDataObject[] rval;
        WMOHeader wmoHdr = new WMOHeader(data);
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        if (wmoHdr.isValid()) {
            byte[] pdata = BinLightningDecoder.extractPData(wmoHdr, data);
            if (pdata != null) {
                try {
                    rval = decodeInternal(fileName, pdata);
                } catch (Exception e) {
                    error(e, headers, wmoHdr);
                    rval = new PluginDataObject[0];
                }
            } else {
                warn("Unable to separate data from headers", fileName, wmoHdr);
                rval = new PluginDataObject[0];
            }
        } else {
            warn("Invalid WMO header", fileName, wmoHdr);
            rval = new PluginDataObject[0];
        }
        return rval;
    }

    /**
     * Display warning message with file and header names
     * 
     * @param msg
     * @param fileName
     * @param wmoHdr
     */
    private void warn(String msg, String fileName, WMOHeader wmoHdr) {
        log.warn(msg + ". File: " + fileName + ", WMO Header: " + wmoHdr);
    }

    /**
     * Display error message with file and header names
     * 
     * @param e
     * @param headers
     * @param wmoHdr
     */
    private void error(Exception e, Headers headers, WMOHeader wmoHdr) {
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        log.error(e.getLocalizedMessage() + ". File: " + fileName
                + ", WMO Header: " + wmoHdr, e);
    }


    /**
     * @param fileName
     * @param pdata
     *            data after WMO header is removed
     * @return
     * @throws DecoderException
     */
    private PluginDataObject[] decodeInternal(String fileName, byte[] pdata)
            throws DecoderException {
        List<LightningStrikePoint> decodeStrikes = decodeStrikes(fileName,
                pdata);
        BinLightningRecord record = new BinLightningRecord(decodeStrikes);
        return new PluginDataObject[] { record };
    }


    /**
     * Extract strike data from raw binary
     * 
     * @param fileName
     * @param pdata
     * @return
     * @throws DecoderException
     */
    private List<LightningStrikePoint> decodeStrikes(String fileName,
            byte[] pdata) throws DecoderException {
        List<LightningStrikePoint> rval = new ArrayList<LightningStrikePoint>();
        ChecksumByteBuffer buff = new ChecksumByteBuffer(pdata);
        while (buff.position() < buff.size()) {
            int totalBytes = UnsignedNumbers.ushortToInt(buff.getShort());
            if (totalBytes > (buff.size() - buff.position())) {
                log.error("Truncated total lightning packet in file: "
                        + fileName);
                break;
            }
            /* start flash packet */
            buff.resetPacketSum();
            /* discard flash packet size byte */
            buff.get();

            LtgStrikeType flashType = getStrikeType(buff.get());
            LightningStrikePoint strike = new LightningStrikePoint(null,
                    LtgMsgType.TOTAL_LIGHTNING);
            strike.setLightSource(DATA_SOURCE);
            strike.setType(flashType);
            decodeCommonFields(strike, buff);

            int pulseCount = UnsignedNumbers.ubyteToShort(buff.get());
            strike.setPulseCount(pulseCount);
            checkSum(buff, false);

            List<LightningPulsePoint> pulses = new ArrayList<LightningPulsePoint>(
                    pulseCount);
            for (int i = 0; i < pulseCount; ++i) {
                /* discard size of pulse packet (always 26) */
                buff.get();
                LtgPulseType pulseType = getPulseType(buff.get());
                LightningPulsePoint pulse = new LightningPulsePoint(null,
                        pulseType);
                decodeCommonFields(pulse, buff);
                /* discard pulse count (already set in strike) */
                buff.get();
                checkSum(buff, false);
                pulses.add(pulse);
            }
            strike.setPulses(pulses);
            checkSum(buff, true);
            rval.add(strike);
        }
        return rval;
    }

    /**
     * Extract fields common to both strikes and pulses
     * 
     * @param point
     * @param buff
     */
    private static void decodeCommonFields(BaseLightningPoint point,
            ChecksumByteBuffer buff) {
        point.setTime(getTime(buff));
        point.setLatitude(getDouble(buff, LONLAT_SCALE_FACTOR));
        point.setLongitude(getDouble(buff, LONLAT_SCALE_FACTOR));
        point.setStrikeStrength(getKiloAmps(buff.getInt()));
        /* discard reserved byte */
        buff.get();
        point.setElevation(getMeters(buff.getShort()));
        point.setSensorCount(UnsignedNumbers.ubyteToShort(buff.get()));
    }

    /**
     * Create calendar from 4 byte UNIX time and 2 byte millisecond addition
     * 
     * @param buff
     * @return
     */
    private static Calendar getTime(ChecksumByteBuffer buff) {
        long unixTime = UnsignedNumbers.uintToLong(buff.getInt());
        int additionalMillis = UnsignedNumbers.ushortToInt(buff.getShort());
        long totalMillis = (unixTime * TimeUtil.MILLIS_PER_SECOND)
                + additionalMillis;
        return TimeUtil.newGmtCalendar(new Date(totalMillis));
    }

    /**
     * Ensure data integrity, resets appropriate sum(s) in buffer after check
     * 
     * @param buff
     * @param total
     *            true if total sum should be checked, otherwise checks packet
     *            sum
     * @throws DecoderException
     *             if check fails
     */
    private static void checkSum(ChecksumByteBuffer buff, boolean total)
            throws DecoderException {
        long rawsum = total ? buff.getTotalSum() : buff.getPacketSum();
        /* convert to overflowed unsigned byte */
        rawsum &= 0xFF;
        /* checksum algorithm from total lightning spec */
        long mungedSum = (256 - rawsum) & 0xFF;
        /* get expected after sum so it is not reflected in sum */
        long expected = UnsignedNumbers.ubyteToShort(buff.get());
        if (mungedSum != expected) {
            throw new DecoderException("Checksum failed: expected " + expected
                    + " got " + mungedSum);
        }
        if (total) {
            buff.resetAllSums();
        } else {
            buff.resetPacketSum();
        }
    }

    /**
     * Get scaled double from 4 byte integer field
     * 
     * @param buff
     * @param scaleFactor
     * @return
     */
    private static double getDouble(ChecksumByteBuffer buff, double scaleFactor) {
        int raw = buff.getInt();
        return raw * scaleFactor;
    }

    /**
     * Convert amps to kiloamps
     * 
     * @param amps
     * @return
     */
    private static double getKiloAmps(int amps) {
        return amps / AMPS_PER_KILO_AMP;
    }

    /**
     * Convert kilometers to meters
     * 
     * @param kilometers
     * @return
     */
    private static double getMeters(short kilometers) {
        return kilometers * METERS_PER_KILOMETER;
    }

    /**
     * Map strike byte to internal enum
     * 
     * @param type
     * @return
     * @throws DecoderException
     */
    public static LtgStrikeType getStrikeType(byte type)
            throws DecoderException {
        switch (type) {
        case CLOUD_TO_GROUND_TYPE:
            return LtgStrikeType.CLOUD_TO_GROUND;
        case CLOUD_TO_CLOUD_TYPE:
            return LtgStrikeType.CLOUD_TO_CLOUD;
        case KEEP_ALIVE_TYPE:
            return LtgStrikeType.KEEP_ALIVE;
        }
        throw new DecoderException("Unknown flash type: " + type);
    }

    /**
     * Map pulse byte to internal enum
     * 
     * @param type
     * @return
     * @throws DecoderException
     */
    public static LtgPulseType getPulseType(byte type) throws DecoderException {
        switch (type) {
        case RETURN_STROKE_TYPE:
            return LtgPulseType.RETURN_STROKE;
        case NON_RETURN_STROKE_TYPE:
            return LtgPulseType.NON_RETURN_STROKE;
        case KEEP_ALIVE_TYPE:
            return LtgPulseType.KEEP_ALIVE;
        }
        throw new DecoderException("Unknown pulse type: " + type);
    }

}
