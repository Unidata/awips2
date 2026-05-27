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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;

/**
 * Decode the MAROB (Marine Observation) data. This format is very similar to
 * SHIP Synoptic (FM-13)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010     391        jkorman     Initial coding.
 * 20071217     453        jkorman     Added code to report MAROB report type.
 * Sep 30, 2014 3629       mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()} calls.
 * Dec 17, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class MAROBSynopticDecoder extends SHIPSynopticDecoder {

    // The logger
    private Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Create and set up the MAROB synoptic decoder.
     */
    public MAROBSynopticDecoder() {
        super();
        reportPrefix = "MAROB";
        addSectionDecoder(new SynopticSec1Decoder(this), 1);
        addSectionDecoder(new SynopticSec2Decoder(this), 2);
        addSectionDecoder(new SynopticSec3Decoder(this), 3);
        addSectionDecoder(new SynopticSec4Decoder(this), 4);
    }

    /**
     * Perform the section 0 decode for Land Synoptic reports.
     * 
     * @throws DecoderException
     */
    protected void decodeSection0() throws DecoderException {
        boolean isValid = false;

        String element = reportParser.getElement();
        isValid = reportPrefix.equals(element);
        if (isValid) {
            reportParser.next();
            setReportIdentifier(reportParser.getElement());
            reportParser.next();
            element = reportParser.getElement();
            if (element != null
                    && ISynoptic.YYGGI_SUB_W.matcher(element).find()) {
                try {
                    Integer month = getHeader().getMonth();
                    if (month != -1) {
                        setObsMonth(month);
                    }

                    Integer year = getHeader().getYear();
                    if (year != -1) {
                        setObsYear(year);
                    }
                    Integer val = getInt(element, 0, 2);
                    setObsDay(val);

                    val = getInt(element, 2, 4);
                    setObsHour(val);

                    val = getInt(element, 4, 5);
                    setISubw(val);
                } catch (NumberFormatException nfe) {

                }
                isValid = true;
            } else {
                logger.error("BAD:YYGGI_SUB_W : " + reportParser.getReport());
                clearSectionDecoders();
                return;
            }
            logger.info("<-------" + getReportIdentifier()
                    + "---------------->");
            decodeLatitude();
            decodeLongitude();
            if ((shipLatitude == null) || (shipLongitude == null)) {
                clearSectionDecoders();
                return;
            }
            adjustLatLon();
        }
    }

    /**
     * Consolidate report gathers together all of the data decoded in the
     * decoder and any sub-decoders used. Any subclass overriding this method
     * must be sure to call back to this method first.
     * 
     * @return The decoded data.
     */
    protected PluginDataObject consolidateReport() {
        ObsCommon report = (ObsCommon) super.consolidateReport();
        // Need to override the reportType.
        if (report != null) {
            report.setReportType(SYNOPTIC_MAROB);
        }
        return report;
    }
}
