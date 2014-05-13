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
package com.raytheon.edex.plugin.sfcobs.decoder;

import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.SPACE_CHAR;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.sfcobs.decoder.buoy.DRIBUSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.metar.METARDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.CMANSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.LandSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.MAROBSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.MobileSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SHIPSynopticDecoder;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * This factory examines the incoming observation data to determine the proper
 * decoder strategy to be used. METAR and SPECI data is recognized in the
 * message separator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SfcObsDecoderFactory {
    /** The logger */
    private static Log logger = LogFactory.getLog(SfcObsDecoderFactory.class);

    private static final Map<String, Class<? extends ISfcObsDecoder>> DECODER_MAP = new HashMap<String, Class<? extends ISfcObsDecoder>>();
    static {
        DECODER_MAP.put("AAXX", LandSynopticDecoder.class);
        DECODER_MAP.put("BBXX", SHIPSynopticDecoder.class);
        DECODER_MAP.put("OOXX", MobileSynopticDecoder.class);
        DECODER_MAP.put("CMAN", CMANSynopticDecoder.class);
        DECODER_MAP.put("ZZYY", DRIBUSynopticDecoder.class);
        DECODER_MAP.put("MAROB", MAROBSynopticDecoder.class);
        DECODER_MAP.put("METAR", METARDecoder.class);
        DECODER_MAP.put("SPECI", METARDecoder.class);
    }

    /**
     * Use the embedded report type from the next available report in the
     * separator to construct the decoder specific to that data type.
     * 
     * @param separator
     *            The separator containing the message reports.
     * @return An ISfcObsDecoder instance.
     */
    public static ISfcObsDecoder getDecoderInstance(WMOHeader header,
            String reportData) {
        ISfcObsDecoder decoder = null;

        // Check if report is a String, null check for free!
        if (reportData instanceof String) {
            String report = reportData;
            // Find the first space
            int spcPos = report.indexOf(SPACE_CHAR);
            if (spcPos > 0) {
                String reportHdr = report.substring(0, spcPos);
                Class<? extends ISfcObsDecoder> clazz = DECODER_MAP
                        .get(reportHdr);
                if (clazz != null) {
                    try {
                        decoder = clazz.newInstance();
                    } catch (InstantiationException e) {
                        logger.error(e);
                    } catch (IllegalAccessException e) {
                        logger.error(e);
                    }
                }
                if (decoder != null) {
                    decoder.setReportData(report);
                    decoder.setHeader(header);
                }
            }
        }

        return decoder;
    }
}
