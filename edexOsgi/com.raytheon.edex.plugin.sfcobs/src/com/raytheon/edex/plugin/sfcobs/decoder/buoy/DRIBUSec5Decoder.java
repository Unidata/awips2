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
import com.raytheon.edex.plugin.sfcobs.decoder.ReportParser;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSectionDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;

/**
 * Decode buoy section 5 data. Section 5 data is specific to various regions in
 * the world. A concrete implementation of this class must be created for each
 * region to be decoded.
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
public class DRIBUSec5Decoder extends AbstractSectionDecoder {

    /**
     * 
     * @param parent
     */
    public DRIBUSec5Decoder(AbstractSynopticDecoder parent) {
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
        // String element = null;

        while (true) {
            // if we run out of data, exit.
            if (reportParser.next()) {
                // if ((element = reportParser.getElement()) == null) {
                if ((reportParser.getElement()) == null) {
                    break;
                }
            } else {
                break;
            }

            // String s = element.substring(0, 1);
        } // while

    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
    }

    /**
     * Populate an ObsCommon object with the decoded data from this section.
     * 
     * @param receiver
     *            An ObsCommon to receive the decoded data.
     * @return The populated receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {
        return receiver;
    }
}
