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
package com.raytheon.edex.plugin.sfcobs.decoder.buoy;

import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.DataItem;
import com.raytheon.edex.plugin.sfcobs.decoder.ReportParser;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSectionDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticGroups;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Decode DRIBU section 1 data. The data are similar to Synoptic Section 1
 * except the wind data is in group 0.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * Sep 18, 2014       3627 mapeters    Updated deprecated {@link TimeTools} usage.
 * Sep 26, 2014       3629 mapeters    Replaced static imports.
 * Sep 30, 2014       3629 mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()} calls.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DRIBUSec1Decoder extends AbstractSectionDecoder {

    public static final String SEC_1_PATTERN = "111[/0-9]{2}";

    private Integer windDirection = null;

    private Double windSpeed = null;

    private DataItem airTemp = null;

    private DataItem dewTemp = null;

    private DataItem relHumid = null;

    private DataItem stationPressure = null;

    private DataItem seaLevelPressure = null;

    private DataItem pressureChange = null;

    private DataItem changeCharacter = null;

    /**
     * 
     * @param parent
     */
    public DRIBUSec1Decoder(AbstractSynopticDecoder parent) {
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
        Pattern sec1 = Pattern.compile(SEC_1_PATTERN);
        init();
        if (reportParser == null) {
            // nothing to do.
            return;
        }
        String element = null;
        if (reportParser.positionTo(sec1)) {
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }

                if (DRIBUSec2Decoder.SEC_2_PATTERN.matcher(element).find()) {
                    break;
                } else if (ISynoptic.SEC_3_LEAD_PATTERN.matcher(element).find()) {
                    break;
                } else if (ISynoptic.SEC_4_LEAD_PATTERN.matcher(element).find()) {
                    break;
                } else if (ISynoptic.SEC_5_LEAD_PATTERN.matcher(element).find()) {
                    break;
                }

                String s = element.substring(0, 1);
                if ("0".equals(s) && doGroup(0)) {
                    Integer temp = AbstractSfcObsDecoder.getInt(element, 1, 3);
                    if ((temp != null) && (temp >= 0)) {
                        windDirection = temp * 10;
                    }
                    temp = AbstractSfcObsDecoder.getInt(element, 3, 5);
                    if ((temp != null) && (temp >= 0)) {
                        windSpeed = temp.doubleValue();
                    }
                    closeGroup(0);
                } else if ("1".equals(s) && doGroup(1)) {
                    airTemp = SynopticGroups.decodeTemperature(element, 1);
                    closeGroup(1);
                } else if ("2".equals(s) && doGroup(2)) {
                    if ((dewTemp = SynopticGroups.decodeTemperature(element, 1)) == null) {
                        // no dew point, so check if relative humidity.
                        relHumid = SynopticGroups.decodeRelativeHumidity(
                                element, 1);
                    }
                } else if ("3".equals(s) && doGroup(3)) {
                    stationPressure = SynopticGroups.decodeStationPressure(
                            element, 1);
                    closeGroup(3);
                } else if ("4".equals(s) && doGroup(4)) {
                    seaLevelPressure = SynopticGroups.decodeSeaLevelPressure(
                            element, 1);
                    closeGroup(4);
                } else if ("5".equals(s) && doGroup(5)) {
                    Integer val = AbstractSfcObsDecoder.getInt(element, 1, 2);
                    changeCharacter = new DataItem("changeCharacter");
                    changeCharacter.setDataValue(val.doubleValue());
                    changeCharacter
                            .setDataPeriod(3 * TimeUtil.SECONDS_PER_HOUR);
                    pressureChange = SynopticGroups.decodePressureChange(
                            element, 1);
                    closeGroup(5);
                }
            } // while
        }
    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        windDirection = null;
        windSpeed = null;
        airTemp = null;
        dewTemp = null;
        relHumid = null;
        stationPressure = null;
        seaLevelPressure = null;
        pressureChange = null;
        changeCharacter = null;
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

            int iSubW = decoderParent.getISubw();
            double conversion = 1.0;
            if ((iSubW == 3) || (iSubW == 4)) {
                conversion = 0.5145;
            }

            receiver.setTemp(DataItem.getValue(airTemp, ignore));
            receiver.setDwpt(DataItem.getValue(dewTemp, ignore));
            receiver.setHumidity(DataItem.getValue(relHumid, ignore));

            if ((windDirection != null) && (windSpeed != null)) {
                receiver.setWindDirection(windDirection);
                receiver.setWindSpeed(windSpeed * conversion);
            }

            if (seaLevelPressure != null) {
                receiver.setPressureSealevel(DataItem.getValue(
                        seaLevelPressure, ignore).intValue());
            }
            if (stationPressure != null) {
                receiver.setPressureStation(DataItem.getValue(stationPressure,
                        ignore).intValue());
            }

            if ((pressureChange != null) && (changeCharacter != null)) {
                receiver.setPressChange3Hr(DataItem.getValue(pressureChange,
                        ignore));
                Double v = DataItem.getValue(changeCharacter, ignore);
                if (v != null) {
                    receiver.setPressChangeChar(v.intValue());
                } else {
                    receiver.setPressChangeChar(null);
                }
            }
        }
        return receiver;
    }

}
