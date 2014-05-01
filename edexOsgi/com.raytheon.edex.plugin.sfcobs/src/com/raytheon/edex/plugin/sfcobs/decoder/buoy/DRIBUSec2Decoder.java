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

import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.getInt;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.matchElement;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_3_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_4_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_LEAD;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_ERROR;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_MISSING;

import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSectionDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticGroups;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.edex.decodertools.core.DataItem;
import com.raytheon.uf.edex.decodertools.core.ReportParser;

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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DRIBUSec2Decoder extends AbstractSectionDecoder {

    public static final String SEC_2_PATTERN = "222[/0-9]{2}";

    private DataItem seaTemp = null;

    // high resolution wave period (second)
    private Integer windWavePeriod_lo = null;

    // low resolution wave period (1 meter)
    private Integer windWaveHeight_lo = null;

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
     * Decode this section data.
     * 
     * @param reportParser
     *            Parser containing elements of the observation to be decoded.
     * @throws DecoderException
     *             Thrown when an relevant error has occurred.
     */
    public void decode(ReportParser reportParser) throws DecoderException {
        Pattern sec2 = Pattern.compile(SEC_2_PATTERN);
        init();
        if (reportParser == null) {
            // nothing to do.
            return;
        }
        String element = null;

        if (reportParser.positionTo(sec2)) {
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }

                if (matchElement(element, SEC_3_LEAD)) {
                    break;
                } else if (matchElement(element, SEC_4_LEAD)) {
                    break;
                } else if (matchElement(element, SEC_5_LEAD)) {
                    break;
                }

                if ("0".equals(element.substring(0, 1))) {
                    seaTemp = SynopticGroups.decodeTemperature(element, 2);
                } else if ("1".equals(element.substring(0, 1))) {
                    windWavePeriod_lo = getInt(element, 1, 3);
                    windWaveHeight_lo = getInt(element, 3, 5);
                } else if ("20".equals(element.substring(0, 2))) {
                    windWavePeriod_hi = getInt(element, 1, 3);
                    if ((windWavePeriod_hi != null) && (windWavePeriod_hi >= 0)) {
                        // TODO : For now the finest granularity in time is one
                        // second,
                        // so am reporting the period rounded to the nearest
                        // second.
                        windWavePeriod_hi = (int) Math
                                .round(windWavePeriod_hi / 10.0);
                    }
                } else if ("21".equals(element.substring(0, 2))) {
                    windWaveHeight_hi = getInt(element, 3, 5);
                }
            } // while
        }

    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        seaTemp = null;
        windWavePeriod_lo = null;
        windWaveHeight_lo = null;
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
        final Double[] ignore = { VAL_ERROR.doubleValue(),
                VAL_MISSING.doubleValue() };

        if (receiver != null) {
            receiver.setSeaTemp(DataItem.getValue(seaTemp, ignore));

            if ((windWavePeriod_hi != null) || (windWaveHeight_hi != null)) {
                if (windWavePeriod_hi >= 0) {
                    receiver.setWindWavePeriod(windWavePeriod_hi);
                }
                if (windWaveHeight_hi >= 0) {
                    receiver.setWindWaveHeight(windWaveHeight_hi.doubleValue() * 0.5);
                }
            }
        }
        return receiver;
    }
}
