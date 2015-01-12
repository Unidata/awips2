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

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.ReportParser;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSectionDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;

/**
 * Decode the Drifting buoy section 3 data. The current decoders skip Buoy
 * section 3 data, so this does nothing for now.
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * Sep 30, 2014       3629 mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()} calls.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DRIBUSec3Decoder extends AbstractSectionDecoder {

    /**
     * 
     * @param parent
     */
    public DRIBUSec3Decoder(AbstractSynopticDecoder parent) {
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
        String element = null;
        if (reportParser.positionTo(ISynoptic.SEC_3_LEAD_STRING)) {
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }

                if (ISynoptic.SEC_4_LEAD_PATTERN.matcher(element).find()) {
                    break;
                } else if (ISynoptic.SEC_5_LEAD_PATTERN.matcher(element).find()) {
                    break;
                }

            } // while
        }
    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
    }

    /**
     * Populate an ObsCommon object with the decoded data from this section.
     * Nothing right now.
     * 
     * @param receiver
     *            An ObsCommon to receive the decoded data.
     * @return The populated receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {
        // final Double[] ignore = { VAL_ERROR.doubleValue(),
        // VAL_MISSING.doubleValue() };

        if (receiver != null) {
        }
        return receiver;
    }

}
