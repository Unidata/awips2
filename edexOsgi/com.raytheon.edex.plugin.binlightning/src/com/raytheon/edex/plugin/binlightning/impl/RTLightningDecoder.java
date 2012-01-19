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
package com.raytheon.edex.plugin.binlightning.impl;

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.IBinDataSource;

/**
 * Decode one or more Real Time Flash lightning observations. Decode algorithm
 * is taken from the NWS D2D binary lightning decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20070821            379 jkorman     Added default strike type.
 * 20080823            379 jkorman     getRTLat was using 24 bits instead of 23.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class RTLightningDecoder extends BaseLightningDecoder {
    /**
     * Construct an instance of this decoder, and decode the message data.
     * 
     * @param msgData
     *            Message data.
     * @param count
     *            Number of flash reports contained in the message part.
     */
    public RTLightningDecoder(IBinDataSource msgData, int count) {
        super();
        doDecode(msgData, count);
    }

    /**
     * Perform the message decode for real time strike message.
     * 
     * @param msgData
     *            Message data.
     * @param count
     *            Number of flash reports contained in the message part.
     */
    private void doDecode(IBinDataSource msgData, int count) {
        if (msgData.available(TIME_SIZE + (RT_MSG_SIZE * count))) {
            BasePoint base = parseDate(msgData);
            // for now just consume some data
            for (int i = 0; i < count; i++) {
                long part = msgData.getU32();

                double lon = getRTLon(part);
                double strength = getSignalStrength(part);

                part = msgData.getU32();

                double lat = getRTLat(part);
                int strikeCount = getMult(part);

                LightningStrikePoint strikeData = new LightningStrikePoint(
                        base, lat, lon, LtgMsgType.STRIKE_MSG_RT);

                strikeData.setStrikeStrength(strength);
                strikeData.setStrikeCount(strikeCount);
                // *****
                // NCDC documents indicate that RT data can report both CC/CG
                // but haven't seen any data nor is it in the D2D decoders. Set
                // to default for now.
                // *****
                strikeData.setType(DEFAULT_FLASH_TYPE);
                strikeData.setMillis(0);

                addStrike(strikeData);
            }
        } else {
            setError(IBinLightningDecoder.NO_TIME_INFO);
        }
    }

    /**
     * Decode the Real Time lightning longitude. Data is in the lower 24 bits.
     * 
     * @param msgPart
     *            Unsigned 32 bit value holding the coded longitude.
     * @return The decoded longitude.
     */
    private double getRTLon(long msgPart) {
        int value = (int) (msgPart & 0xFFFFFFL);
        double lon = (value / 16777216.0 * 360.0) - 180.0;

        return lon;
    }

    /**
     * Decode the Real Time lightning latitude. Data is in the lower 24 bits.
     * 
     * @param msgPart
     *            Unsigned 32 bit value holding the coded latitude.
     * @return The decoded latitude.
     */
    private double getRTLat(long msgPart) {
        int value = (int) (msgPart & 0x7FFFFFL);

        double lat = (value / 16777216.0 * 360.0) - 90.0;

        return lat;
    }

    /**
     * Decode the Real Time lightning signal strength. Data is in the upper 8
     * bits.
     * 
     * @param msgPart
     *            Unsigned 32 bit value holding the coded strength.
     * @return The decoded signal strength.
     */
    private double getSignalStrength(long msgPart) {
        final int SIGNMAG = 128;

        double sigStrength = 0;
        int temp = (int) ((msgPart >> 24) & 0xFF);
        // if the temp strength is greater than 127, the result should be
        // negative.
        if (temp >= SIGNMAG) {
            temp = SIGNMAG - temp;
        }
        // Make signal strength in the range -254..254
        sigStrength = temp * 2;

        return sigStrength;
    }

    /**
     * Decode the number of strikes within the strike record. Data is encoded in
     * bits 24..27.
     * 
     * @param msgPart
     *            Unsigned 32 bit value holding the count.
     * @return The number of strikes within the strike record.
     */
    private int getMult(long msgPart) {
        // TODO : Need to check this! There is data in bit 28-31 but none in
        // 24..27 in current data.
        // NCDC document 9603 indicates that RT_FLASH doesn't report mult so
        // always zero would be correct
        int temp = (int) ((msgPart >> 24) & 0x0F);

        return temp;
    }

}
