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
import com.raytheon.edex.plugin.sfcobs.decoder.ReportParser;
import com.raytheon.uf.common.dataplugin.sfcobs.AncCloud;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;

/**
 * Decode synoptic section 4 data. This section has a single group which
 * describes a cloud layer at a level below that of the observing station.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * Sep 30, 2014       3629 mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()} calls.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SynopticSec4Decoder extends AbstractSectionDecoder {

    // N' Code Table 2700
    private Integer cloudAmount = null;

    // C' Code Table 0500
    private Integer cloudGenus = null;

    // H'H' Altitude of cloud layer reported by C' in hundreds of meters.
    private Integer cloudAltitude = null;

    // Ct Code Table 0552
    private Integer cloudDescription = null;

    /**
     * 
     * @param parent
     */
    public SynopticSec4Decoder(AbstractSynopticDecoder parent) {
        super(parent);
    }

    /**
     * Decode the section 4 data. This is the only group in section 4.
     * 
     * <pre>
     *  444 N'C'H'H'Ct
     *  
     *  N'     Code Table 2700
     *  C'     Code Table 0500
     *  H'H'   Altitude of cloud layer reported by C' in hundreds of meters.
     *  Ct     Code Table 0552
     * </pre>
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
        if (reportParser.positionTo(ISynoptic.SEC_4_LEAD_STRING)) {

            if (reportParser.next()) {
                String element = reportParser.getElement();
                if (element != null
                        && ISynoptic.GENERAL_GROUP.matcher(element).find()) {
                    cloudAmount = AbstractSfcObsDecoder.getInt(element, 0, 1);
                    cloudGenus = AbstractSfcObsDecoder.getInt(element, 1, 2);
                    cloudAltitude = AbstractSfcObsDecoder.getInt(element, 2, 4);
                    cloudDescription = AbstractSfcObsDecoder.getInt(element, 4,
                            5);
                }
            }
        }
    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        cloudAmount = null;
        cloudGenus = null;
        cloudAltitude = null;
        cloudDescription = null;
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
            // Set to null here. If any data needs to be added, then the
            // object will be created.
            AncCloud layer = null;

            if (cloudAmount != null) {
                if (layer == null) {
                    layer = new AncCloud(4);
                    receiver.addCloud(layer);
                }
                layer.setCloudAmount(cloudAmount);
            }
            if (cloudGenus != null) {
                if (layer == null) {
                    layer = new AncCloud(4);
                    receiver.addCloud(layer);
                }
                layer.setCloudgenus(cloudGenus);
            }
            if (cloudAltitude != null) {
                if (layer == null) {
                    layer = new AncCloud(4);
                    receiver.addCloud(layer);
                }
                layer.setCloudHeight(cloudAltitude);
            }
            if (cloudDescription != null) {
                if (layer == null) {
                    layer = new AncCloud(4);
                    receiver.addCloud(layer);
                }
                layer.setCloudObsType(cloudDescription);
            }
        }
        return receiver;
    }

}
