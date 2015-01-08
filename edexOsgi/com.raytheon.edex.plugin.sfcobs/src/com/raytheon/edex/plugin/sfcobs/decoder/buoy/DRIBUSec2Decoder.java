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
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * Decode synoptic section 2 data. This section has a single group which
 * describes a cloud layer at a level below that of the observing station. The
 * wave observation data in this group consists of two height and period data,
 * low resolution is to the nearest meter and second, the high resolution data
 * is to the nearest 1/10th meter and 1/10th second. The heights are reported
 * out in the AncWave object to the nearest millimeter, however the period is
 * rounded to the nearest second.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * Sep 26, 2014       3629 mapeters    Removed unused fields, replaced static imports.
 * Sep 30, 2014       3629 mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()}
 *                                     calls, changed SEC_2_PATTERN from String to Pattern.
 * Jan 08, 2015       3897 nabowle     Better handle invalid data with missing
 *                                     section identifiers.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DRIBUSec2Decoder extends AbstractSectionDecoder {

    // maximum number of elements after the Section Identifier.
    private static final int MAX_ELEMS = 4;

    public static final Pattern SEC_2_PATTERN = Pattern.compile("222[/0-9]{2}");

    public static final Pattern SEA_TEMP_PATTERN = Pattern
            .compile("0[01][/0-9]{3}");

    private DataItem seaTemp = null;

    // high resolution wave period (1/10th second)
    private Integer windWavePeriod_hi = null;

    // high resolution wave height (1/10th meter)
    private Integer windWaveHeight_hi = null;

    /**
     *
     * @param parent
     */
    public DRIBUSec2Decoder(AbstractSynopticDecoder parent) {
        super(parent);
    }

    /**
     * Decode this section data. Any or all of the Section 2 elements may be in
     * the section, with an element being omitted when there is no data. If
     * there is no Section 2 data, the whole section is omitted.
     *
     * The fields take the following form (from the Manual on Codes WMO 306
     * Volume I.1 Part A):
     *
     * <pre>
     *    ID     SeaTemp   Wave Period/Height   Wave Period       Wave Height
     *           Celsius    Seconds/.5 Meter      .1 Sec            .1 Meter
     * (222QdQx 0snTwTwTw    1PwaPwaHwaHwa      20PwaPwaPwa       21HwaHwaHwa)
     * </pre>
     *
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
        String element = null;

        if (reportParser.positionTo(SEC_2_PATTERN)) {
            int fieldCount = 0;
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }

                if (ISynoptic.SEC_3_LEAD_PATTERN.matcher(element).find()) {
                    break;
                } else if (ISynoptic.SEC_4_LEAD_PATTERN.matcher(element).find()) {
                    break;
                } else if (ISynoptic.SEC_5_LEAD_PATTERN.matcher(element).find()) {
                    break;
                }

                // Try to handle cases where a Section Lead pattern is missing
                // and the parser continues into the unmarked section.
                if (++fieldCount > MAX_ELEMS) {
                    throw new DecoderException(
                            "Invalid Section 2 data due to too many elements.");
                }

                if (SEA_TEMP_PATTERN.matcher(element).matches()) {
                    seaTemp = SynopticGroups.decodeTemperature(element, 2);
                } else if ("20".equals(element.substring(0, 2))) {
                    windWavePeriod_hi = AbstractSfcObsDecoder.getInt(element,
                            1, 3);
                    if ((windWavePeriod_hi != null) && (windWavePeriod_hi >= 0)) {
                        // TODO : For now the finest granularity in time is one
                        // second,
                        // so am reporting the period rounded to the nearest
                        // second.
                        windWavePeriod_hi = (int) Math
                                .round(windWavePeriod_hi / 10.0);
                    }
                } else if ("21".equals(element.substring(0, 2))) {
                    windWaveHeight_hi = AbstractSfcObsDecoder.getInt(element,
                            3, 5);
                } else if (!element.startsWith("1")) {
                    throw new DecoderException(
                            "Invalid Section 2 data due to an invalid element ["
                                    + element + "].");
                }
            } // while
        }

    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        seaTemp = null;
        windWavePeriod_hi = null;
        windWaveHeight_hi = null;
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
            receiver.setSeaTemp(DataItem.getValue(seaTemp, ignore));

            if (windWavePeriod_hi != null && windWavePeriod_hi >= 0) {
                receiver.setWindWavePeriod(windWavePeriod_hi);
            }
            if (windWaveHeight_hi != null && windWaveHeight_hi >= 0) {
                receiver.setWindWaveHeight(windWaveHeight_hi.doubleValue() * 0.5);
            }
        }
        return receiver;
    }
}
