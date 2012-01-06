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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic.regional;

import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.getInt;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.matchElement;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_72_CMAXMIN;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_72_CTEMP;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_LEAD;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.sfcobs.AncPrecip;
import com.raytheon.uf.common.dataplugin.sfcobs.AncTemp;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticGroups;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticSec5Decoder;
import com.raytheon.uf.edex.decodertools.core.DataItem;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.ReportParser;

/**
 * Decode synoptic section 5 regional data for WMO block 72 (United States)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class Sec5Block72Decoder extends SynopticSec5Decoder {

    private DataItem cityTemperature = null;

    private DataItem cityMaxTemperature = null;

    private DataItem cityMinTemperature = null;

    private DataItem city24HrPrecip = null;

    /**
     * Construct this decoder using a specified decoder parent.
     * 
     * @param parent
     *            The synoptic decoder parent.
     */
    public Sec5Block72Decoder(AbstractSynopticDecoder parent) {
        super(parent);
    }

    /**
     * Decode the block 72 (United States regional) section data.
     * 
     * @param reportParser
     *            Parser containing elements of the observation to be decoded.
     * @throws DecoderException
     *             Thrown when an relevant error has occured.
     */
    public void decode(ReportParser reportParser) throws DecoderException {
        init();
        boolean isBlock72Data = false;
        if (decoderParent != null) {
            String stationId = decoderParent.getReportIdentifier();
            if (stationId != null) {
                isBlock72Data = stationId.startsWith("72");
            }
        }

        if (reportParser == null) {
            // nothing to do.
            return;
        }
        if (reportParser.positionTo(SEC_5_LEAD)) {
            String element = null;

            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }
                if (isBlock72Data) {
                    if (matchElement(element, SEC_5_72_CTEMP)) {
                        // City temperature.
                        Double val = decodeFahrenheit(element.substring(1, 4));
                        if (val != null) {
                            cityTemperature = new DataItem("K", "cityTemp", 5);
                            cityTemperature.setDataValue(val);
                            cityTemperature.setDataPeriod(0);
                        }
                    } else if (matchElement(element, SEC_5_72_CMAXMIN)) {
                        // City maximum/minimum temperature.
                        Double val = decodeFahrenheit(element.substring(0, 3));
                        if (val != null) {
                            cityMaxTemperature = new DataItem("K",
                                    "cityMaxTemp", 5);
                            cityMaxTemperature.setDataValue(val);
                            cityMaxTemperature.setDataPeriod(0);
                        }
                        val = decodeFahrenheit(element.substring(3, 6));
                        if (val != null) {
                            cityMinTemperature = new DataItem("K",
                                    "cityMinTemp", 5);
                            cityMinTemperature.setDataValue(val);
                            cityMinTemperature.setDataPeriod(0);
                        }

                    } else if ("2".equals(element.substring(0, 1))) {
                        // City 24 hour precipitation amount.
                        city24HrPrecip = SynopticGroups
                                .decodePrecip(element, 5);
                    }
                }
                if ("9".equals(element.substring(0, 1))) {
                    // This is a 9YYGG group that is decoded elsewhere. We won't
                    // do it again here. per NCEP decoder.
                    reportParser.next();
                    break;
                }
            }
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
        if (receiver != null) {
            if (cityTemperature != null) {
                Double v = cityTemperature.getDataValue();
                if ((v != null) && (v >= 0)) {
                    AncTemp t = new AncTemp();
                    t.setValue(v);
                    t.setTimePeriod(0);
                    t.setObsType(AncTemp.T_US_CITY_TEMP);
                }
            }

            if (cityMaxTemperature != null) {
                Double v = cityMaxTemperature.getDataValue();
                if ((v != null) && (v >= 0)) {
                    AncTemp t = new AncTemp();
                    t.setValue(v);
                    t.setTimePeriod(0);
                    t.setObsType(AncTemp.T_US_CITY_MAX);
                }
            }

            if (cityMinTemperature != null) {
                Double v = cityMinTemperature.getDataValue();
                if ((v != null) && (v >= 0)) {
                    AncTemp t = new AncTemp();
                    t.setValue(v);
                    t.setTimePeriod(0);
                    t.setObsType(AncTemp.T_US_CITY_MIN);
                }
            }
            if (city24HrPrecip != null) {
                Double v = city24HrPrecip.getDataValue();
                if ((v != null) && (v >= 0)) {
                    AncPrecip p = new AncPrecip();
                    p.setPrecipAmount(v);
                    p.setTimePeriod(city24HrPrecip.getDataPeriod());
                    p.setObsType(AncPrecip.PRECIP_TOTAL);
                }
            }
        }
        return receiver;
    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        cityTemperature = null;
        cityMaxTemperature = null;
        cityMinTemperature = null;
        city24HrPrecip = null;
    }

    /**
     * Decode the reported fahrenheit temperatures. Data are converted to
     * Kelvin.
     * 
     * @param element
     *            The encoded data.
     * @return The decoded value. Returns null if no data present.
     */
    private static final Double decodeFahrenheit(String element) {
        Double decodedValue = null;
        int sign = SynopticGroups.getSign(element.charAt(0));
        Integer val = getInt(element, 1, 3);
        if ((val != null) && (val >= 0)) {
            double f = (((double) val * sign) - 32) * 5.0 / 9.0;
            decodedValue = DecoderTools.celsiusToKelvin(f, 1, 1);
        } else {
            decodedValue = null;
        }

        return decodedValue;
    }

}
