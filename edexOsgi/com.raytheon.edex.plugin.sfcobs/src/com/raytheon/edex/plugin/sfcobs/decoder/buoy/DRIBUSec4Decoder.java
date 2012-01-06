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
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_4_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_LEAD;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSectionDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.edex.decodertools.core.ReportParser;

/**
 * Decode synoptic section 4 data. This section has a single group which
 * describes the buoy drift.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DRIBUSec4Decoder extends AbstractSectionDecoder {
    private final double CM_PER_METER = 100.0;

    // Drift speed in cm per sec.
    private Integer driftSpeed = null;

    private Integer driftDirection = null;

    /**
     * 
     * @param parent
     */
    public DRIBUSec4Decoder(AbstractSynopticDecoder parent) {
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
        if (reportParser.positionTo(SEC_4_LEAD)) {
            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }

                if (matchElement(element, SEC_5_LEAD)) {
                    break;
                }

                if ("7".equals(element.substring(0, 1))) {
                    driftSpeed = getInt(element, 1, 3);
                    driftDirection = getInt(element, 3, 5);
                    closeGroup(7);
                }

            } // while
        }

    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        driftSpeed = null;
        driftDirection = null;
    }

    /**
     * Populate an ObsCommon object with this cloud data if it was decoded.
     * 
     * @param receiver
     *            An ObsCommon to receive the cloud data.
     * @return The receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {
        if (receiver != null) {
            if ((driftSpeed != null) && (driftDirection != null)) {
                if (driftSpeed >= 0) {
                    // report drift in meters per second.
                    receiver.setPlatformMovement(driftSpeed / CM_PER_METER);
                }
                if (driftDirection >= 0) {
                    // scale direction by 10.
                    receiver.setPlatformDirection(driftDirection * 10);
                }
            }
        }
        return receiver;
    }
}
