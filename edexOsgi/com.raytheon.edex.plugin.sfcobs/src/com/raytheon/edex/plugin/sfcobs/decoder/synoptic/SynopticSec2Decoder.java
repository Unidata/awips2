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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.DataItem;
import com.raytheon.edex.plugin.sfcobs.decoder.ReportParser;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * Decode synoptic section 2 data. This section has a single group which
 * describes a cloud layer at a level below that of the observing station.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * 20071109            391 jkorman     Added guard for short data.
 * 2013/8			757	   	T. Lee		Checked missing wave height from ship report
 * Sep 26, 2014       3629 mapeters    Replaced static imports.
 * Sep 30, 2014       3629 mapeters    Conformed to changes in {@link ISynoptic} constants.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SynopticSec2Decoder extends AbstractSectionDecoder {
    // Map the ship movement code to velocity - Table
    private static final double[] speeds = { 0.0, 1.38, 4.17, 6.67, 9.17,
            11.81, 14.44, 16.94, 19.58, 20.83 };

    // 0 no movement
    // -1 unknown
    // movement
    private static final int[] dirs = { 0, 45, 90, 135, 180, 225, 270, 315,
            360, -1 };

    private Integer shipDirection = null;

    private Integer shipSpeed = null;

    private DataItem seaTemp = null;

    private DataItem wetBulb = null;

    private Integer wavePeriod = null;

    private Integer waveHeight = null;
    
    private Integer windWavePeriod = null;

    private Integer windWaveHeight = null;

    private Integer swellWave1Period = null;

    private Integer swellWave1Height = null;

    private Integer swellWave1Dir = null;

    private Integer swellWave2Period = null;

    private Integer swellWave2Height = null;

    private Integer swellWave2Dir = null;

    private Integer heightOfWaves = null;

    private String iceCode = null;

    /**
     * 
     * @param parent
     */
    public SynopticSec2Decoder(AbstractSynopticDecoder parent) {
        super(parent);
    }

    /**
     * Decode this section data.
     * 
     * @param reportParser
     *            Parser containing elements of the observation to be decoded.
     * @throws DecoderException
     *             Thrown when an relevant error has occurred.
     */
    public void decode(ReportParser reportParser) throws DecoderException {
        init();
        if (reportParser == null) {
            // nothing to do.
            return;
        }
        if (reportParser.positionTo(ISynoptic.SEC_2_LEAD)) {
            String element = reportParser.getElement();
            // need to decode the Ds vs data
            shipDirection = AbstractSfcObsDecoder.getInt(element, 3, 4);
            shipSpeed = AbstractSfcObsDecoder.getInt(element, 4, 5);
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }

                if (ISynoptic.SEC_3_LEAD_STRING.equals(element)) {
                    break;
                } else if (ISynoptic.SEC_4_LEAD_STRING.equals(element)) {
                    break;
                } else if (ISynoptic.SEC_5_LEAD_STRING.equals(element)) {
                    break;
                } else if ("80000".equals(element)) {
                    break;
                }

                if ("ICE".equals(element)) {
                    // TODO :
                    if (!reportParser.next()) {
                        break;
                    }
                    iceCode = reportParser.getElement();
                    continue;
                } else if ("ICING".equals(element)) {
                    // Not implemented at NCEP - Keep in as a place holder.
                    continue;
                }
                // Can't check for "short" data until here.
                if (element.length() < 5) {
                    // for now skip over possible ill-formed data.
                    continue;
                }

                if ("0".equals(element.substring(0, 1)) && doGroup(0)) {
                    seaTemp = SynopticGroups.decodeTemperature(element, 2);
                    closeGroup(1);
                } else if (doGroup(1) && "1".equals(element.substring(0, 1))) {
                    wavePeriod = AbstractSfcObsDecoder.getInt(element, 1, 3);
                    waveHeight = AbstractSfcObsDecoder.getInt(element, 3, 5);
                    closeGroup(1);
                } else if (doGroup(2) && "2".equals(element.substring(0, 1))) {
                    windWavePeriod = AbstractSfcObsDecoder
                            .getInt(element, 1, 3);
                    windWaveHeight = AbstractSfcObsDecoder
                            .getInt(element, 3, 5);
                    closeGroup(2);
                } else if (doGroup(3) && "3".equals(element.substring(0, 1))) {
                    swellWave1Dir = AbstractSfcObsDecoder.getInt(element, 1, 3);
                    swellWave2Dir = AbstractSfcObsDecoder.getInt(element, 3, 5);
                    closeGroup(3);
                } else if (doGroup(4) && "4".equals(element.substring(0, 1))) {
                    swellWave1Period = AbstractSfcObsDecoder.getInt(element, 1,
                            3);
                    swellWave1Height = AbstractSfcObsDecoder.getInt(element, 3,
                            5);
                    closeGroup(4);
                } else if (doGroup(5) && "5".equals(element.substring(0, 1))) {
                    swellWave2Period = AbstractSfcObsDecoder.getInt(element, 1,
                            3);
                    swellWave2Height = AbstractSfcObsDecoder.getInt(element, 3,
                            5);
                    closeGroup(5);
                } else if (doGroup(6) && "6".equals(element.substring(0, 1))) {
                    // TODO :
                    closeGroup(6);
                } else if (doGroup(7) && "70".equals(element.substring(0, 2))) {
                    heightOfWaves = AbstractSfcObsDecoder.getInt(element, 2, 5);
                    closeGroup(7);
                } else if (doGroup(8) && "8".equals(element.substring(0, 1))) {
                    wetBulb = SynopticGroups.decodeTemperature(element, 2);
                    closeGroup(8);
                }
            }
        }
    }

    /**
     * Clean the slate. Reset all values prior to decode.
     */
    private void init() {
        shipDirection = null;
        shipSpeed = null;
        seaTemp = null;
        wetBulb = null;
        // windWaveMeasured = false;
        windWavePeriod = null;
        windWaveHeight = null;
        swellWave1Period = null;
        swellWave1Height = null;
        swellWave1Dir = null;
        swellWave2Period = null;
        swellWave2Height = null;
        swellWave2Dir = null;
        heightOfWaves = null;
        iceCode = null;
    }

    /**
     * Populate an ObsCommon object with the decoded data from this section.
     * 
     * @param receiver
     *            An ObsCommon to receive the decoded data.
     * @return The populated receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {

        if (receiver != null) {
            final Double[] ignore = {
                    IDecoderConstants.VAL_ERROR.doubleValue(),
                    IDecoderConstants.VAL_MISSING.doubleValue() };

            receiver.setShipIceData(iceCode);
            receiver.setSeaTemp(DataItem.getValue(seaTemp, ignore));
            receiver.setWetBulb(DataItem.getValue(wetBulb, ignore));

            if ((shipDirection != null) && (shipDirection >= 0)) {
                receiver.setPlatformDirection(dirs[shipDirection]);
            }
            if ((shipSpeed != null) && (shipSpeed >= 0)) {
                // platform speed in meters per second.
                receiver.setPlatformMovement(speeds[shipSpeed]);
            }
            if ((heightOfWaves != null) && (heightOfWaves >= 0)) {
                if (heightOfWaves >= 0) {
                    receiver.setHighResWaveHeight(heightOfWaves / 10.0);
                }
            }
           
            if(wavePeriod != null) {
                if(wavePeriod >= 0 && wavePeriod < 99) {
                    receiver.setWavePeriod(wavePeriod);
                } else {
                    wavePeriod = null;
                }
            }
            if((waveHeight != null) && (waveHeight >=0) ) {
                receiver.setWaveHeight(waveHeight.doubleValue() * 0.5);
            }
            if(windWavePeriod != null) {
                if(windWavePeriod >= 0 && windWavePeriod < 99) {
                    receiver.setWindWavePeriod(windWavePeriod);
                    receiver.setWavePeriod(wavePeriod);
                } else {
                    windWavePeriod = null;
                }
            }
            if((windWaveHeight != null) && (windWaveHeight >=0) ) {
                receiver.setWindWaveHeight(windWaveHeight.doubleValue() * 0.5);
            }
            if ((wavePeriod != null) && (waveHeight != null)) {
                if (wavePeriod >= 0 && waveHeight >= 0) {
                    // Wave Height(H)/(1.56(m/s**2) x Period(T)**2)
                    double steepNess = (waveHeight.doubleValue() * 0.5)
                            / (1.56 * (wavePeriod * wavePeriod));
                    receiver.setWaveSteepness(steepNess);
                }
            }
            if ((swellWave1Height != null) && (swellWave1Height >= 0)) {
                receiver.setPrimarySwellWaveHeight(swellWave1Height
                        .doubleValue() * 0.5);
            }
            if ((swellWave2Height != null) && (swellWave2Height >= 0)) {
                receiver.setSecondarySwellWaveHeight(swellWave2Height
                        .doubleValue() * 0.5);
            }
            if ((swellWave1Dir != null) && (swellWave1Dir >= 0) && (swellWave1Dir < 99) ) {
                receiver.setPrimarySwellWaveDir(swellWave1Dir * 10d);
            }
            if ((swellWave2Dir != null) && (swellWave2Dir >= 0) && (swellWave2Dir < 99) ) {
                receiver.setSecondarySwellWaveDir(swellWave2Dir * 10d);
            }
            if ((swellWave1Period != null) && (swellWave1Period >= 0)) {
                receiver.setPrimarySwellWavePeriod(swellWave1Period);
            }
            if ((swellWave2Period != null) && (swellWave2Period >= 0)) {
                receiver.setSecondarySwellWavePeriod(swellWave2Period);
            }
        }
        return receiver;
    }

}
