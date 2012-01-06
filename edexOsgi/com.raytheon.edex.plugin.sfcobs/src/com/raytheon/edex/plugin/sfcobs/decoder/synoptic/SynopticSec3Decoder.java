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

import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.getInt;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.matchElement;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_3_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_4_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_LEAD;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_ERROR;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_MISSING;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.sfcobs.AncPrecip;
import com.raytheon.uf.common.dataplugin.sfcobs.AncPressure;
import com.raytheon.uf.common.dataplugin.sfcobs.AncTemp;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.edex.decodertools.core.DataItem;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.ReportParser;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * (0 . . . .) (1snTxTxTx) (2snTnTnTn) (3Ejjj) (4E'sss)
 * (5j1j2j3j4 (j5j6j7j8j9)) (6RRRtR) (7R24R24R24R24)
 * (8NsChshs) (9SPSPspsp) (80000 (0 . . . .) (1 . . . .) . . . . .)
 * (912ff)
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * 20071203            410 jkorman     JavaDoc complaints.
 * 20080116            798 jkorman     Changed logging levels.
 * 
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SynopticSec3Decoder extends AbstractSectionDecoder {

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private DataItem maxTemperature = null; // (group 1xxxx)

    private DataItem minTemperature = null; // (group 2xxxx)

    private DataItem precip = null; // (group 6xxxx)

    private DataItem precip24 = null; // (group 7xxxx)

    private DataItem pressure24 = null; // 5[89]ppp

    private Double windGust910 = null;

    private Double windGust911 = null;

    private Double windGust912 = null;

    private Integer duration = null;

    /**
     * 
     * @param parent
     */
    public SynopticSec3Decoder(AbstractSynopticDecoder parent) {
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

        int iSubW = decoderParent.getISubw();
        double conversion = 1.0;
        if ((iSubW == 3) || (iSubW == 4)) {
            conversion = 0.5145;
        }

        if (reportParser == null) {
            // nothing to do.
            return;
        }
        String element = null;
        if (reportParser.positionTo(SEC_3_LEAD)) {
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }
                if (SEC_4_LEAD.equals(element)) {
                    break;
                } else if (SEC_5_LEAD.equals(element)) {
                    break;
                } else if ("80000".equals(element)) {
                    break;
                }

                // String s = null;
                if ("1".equals(element.substring(0, 1)) && doGroup(1)) {
                    maxTemperature = SynopticGroups.decodeTemperature(element,
                            3);
                    closeGroup(1);
                } else if ("2".equals(element.substring(0, 1)) && doGroup(2)) {
                    minTemperature = SynopticGroups.decodeTemperature(element,
                            3);
                    closeGroup(2);
                } else if ("3".equals(element.substring(0, 1)) && doGroup(3)) {
                    // TODO :
                    // 3 E - Code table 0901
                    // jjj regional data
                    closeGroup(3);
                } else if ("4".equals(element.substring(0, 1)) && doGroup(4)) {
                    closeGroup(4);
                } else if (matchElement(element, "5[89][0-9/]{3}")
                        && doGroup(5)) {
                    int sign = 0;
                    if (element.charAt(1) == '8') {
                        sign = 1;
                    } else if (element.charAt(1) == '9') {
                        sign = -1;
                    }
                    Integer val = getInt(element, 2, 5);
                    if ((val != null) && (val >= 0)) {
                        pressure24 = new DataItem("Pascals", "24HRChange", 3);
                        pressure24.setDataValue(DecoderTools.hPaToPascals(val
                                * sign));
                        pressure24.setDataPeriod(TimeTools.SECONDS_DAY);
                    }
                    closeGroup(5);
                } else if ("6".equals(element.substring(0, 1)) && doGroup(6)) {
                    precip = SynopticGroups.decodePrecip(element, 3);
                    closeGroup(6);
                } else if ("7".equals(element.substring(0, 1)) && doGroup(7)) {
                    precip24 = SynopticGroups.decodePrecip(element, 3);
                    closeGroup(7);
                } else if ("8".equals(element.substring(0, 1)) && doGroup(8)) {
                    // TODO :
                    // Group 8 doesn't get closed here, there may be more than
                    // one group.
                } else if (matchElement(element, "907\\d{2}")) {
                    Integer temp = getInt(element, 3, 5);
                    if ((temp != null) && (temp >= 0)) {
                        if (temp < 61) {
                            duration = 6 * temp;
                        } else if (temp < 67) {
                            duration = (60 * (temp - 60)) + 360;
                        } else {
                            duration = 1080;
                        }
                    }
                } else if (matchElement(element, "910\\d{2}")) {
                    Integer temp = getInt(element, 3, 5);
                    if ((temp != null) && (temp >= 0)) {
                        windGust910 = temp * conversion;
                    }
                } else if (matchElement(element, "911\\d{2}")) {
                    Integer temp = getInt(element, 3, 5);
                    if ((temp != null) && (temp >= 0)) {

                        windGust911 = temp * conversion;
                    }
                } else if (matchElement(element, "912\\d{2}")) {
                    Integer temp = getInt(element, 3, 5);
                    if ((temp != null) && (temp >= 0)) {
                        windGust912 = temp * conversion;
                        ;
                    }
                } else {
                    logger.debug("Discarding " + element);
                }
            } // while()
        }
    }

    /**
     * Populate an ObsCommon object with the decoded data from this section.
     * 
     * @param receiver
     *            An ObsCommon to receive the decoded data.
     * @return The populated receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {
        final Double[] ignore = { VAL_ERROR.doubleValue(),
                VAL_MISSING.doubleValue() };

        if (maxTemperature != null) {
            AncTemp temp = new AncTemp();
            temp.setObsType(AncTemp.T_MAX_AIR_TEMP);
            temp.setValue(maxTemperature.getDataValue());
            temp.setTimePeriod(0);
            receiver.addTemp(temp);
        }
        if (minTemperature != null) {
            AncTemp temp = new AncTemp();
            temp.setObsType(AncTemp.T_MIN_AIR_TEMP);
            temp.setValue(minTemperature.getDataValue());
            temp.setTimePeriod(0);
            receiver.addTemp(temp);
        }
        if (precip != null) {
            Double v = precip.getDataValue();
            Integer p = precip.getDataPeriod();
            if ((v != null) && (v > 0) && (p != null)) {
                AncPrecip precipT = new AncPrecip();
                precipT.setPrecipAmount(v);
                precipT.setTimePeriod(p);
                precipT.setObsType(AncPrecip.PRECIP_TOTAL);
                receiver.addPrecip(precipT);
            }
        }
        if (precip24 != null) {
            Double v = precip24.getDataValue();
            Integer p = precip24.getDataPeriod();
            if ((v != null) && (v > 0) && (p != null)) {
                AncPrecip precipT = new AncPrecip();
                precipT.setPrecipAmount(v);
                precipT.setTimePeriod(p);
                precipT.setObsType(AncPrecip.PRECIP_TOTAL);
                receiver.addPrecip(precipT);
            }
        }
        if (pressure24 != null) {
            Double v = DataItem.getValue(pressure24, ignore);
            if ((v != null) && (v >= -9999)) {
                AncPressure press = new AncPressure();
                press.setObsType(AncPressure.T_PRESS_CHANGE);
                press.setValue(v.intValue());
                press.setTimePeriod(pressure24.getDataPeriod());
                receiver.addPressure(press);
            }
        }

        // Use the 10 minute gust data if present
        if (windGust910 != null) {
            receiver.setWindGust(windGust910);
        } else if (windGust911 != null) {
            receiver.setWindGust(windGust911);
        } else if (windGust912 != null) {
            receiver.setWindGust(windGust912);
        }

        return receiver;
    }

}
