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
package com.raytheon.edex.transform.shef;

import java.util.Calendar;

import javax.xml.transform.TransformerException;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.transform.shef.obs.SHEF_Obs_Codes;
import com.raytheon.edex.transform.shef.obs.SHEF_SM_Codes;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * Transforms a decoded synoptic observation into a series of SHEF encoded data
 * lines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2008       1659 jkorman     Initial creation
 * ======================================
 * AWIPS2 DR Work
 * 20120918           1185 jkorman     Added save to archive capability.     
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SMToShefTransformer extends AbstractShefTransformer<ObsCommon> {

    private static final String WMO_HEADER_FMT = CRCRLF
            + "SRXX99 %4s %2$td%2$tH%2$tM";

    /**
     * 
     */
    public SMToShefTransformer(String cmdLine) {
        super(cmdLine, WMO_HEADER_FMT);
    }

    /**
     * 
     * @param report
     * @param encoding
     * @return The transformed report.
     * @throws TransformerException
     * @see org.mule.transformers.AbstractTransformer#doTransform(java.lang.Object,
     *      java.lang.String)
     */
    @Override
    public byte[] transformReport(ObsCommon report, Headers headers)
            throws TransformerException {

        // Transformed Synoptic PluginDataObject to SHEF
        byte[] result = null;
        try {
            // Currently returns false, so nothing is encoded at this time.
            if (encodeThisStation(report)) {

                WMOHeader hdr = new WMOHeader(report.getObsText().getBytes());

                StringBuilder sb = makeWMOHeader(openWMOMessage(200),
                        "KWOH", headers, hdr);
                String fileName = makeWMOHeader(new StringBuilder(20), "KWOH",
                        headers, hdr).toString().trim().replace(' ', '_');

                startMessageLine(sb).append(
                        ": SHEF derived data created by SMToShefTransformer");
                startMessageLine(sb).append(": TRACEID = ");
                sb.append(headers.get(DecoderTools.INGEST_FILE_NAME));

                String shef = closeWMOMessage(encodeShef(sb, report, headers))
                        .toString();

                if (options.isOptVerbose()) {
                    logger.info("SynopticToShef: = " + shef);
                }

                archiveSHEFObs(shef, fileName);

                setLastMessage(shef);
                result = shef.getBytes();

            }
        } catch (Exception e) {
            logger.error(e);
        } finally {
            if (result == null) {
                result = new byte[0];
            }
        }

        return result;
    }

    /**
     * 
     * @param buffer
     * @param report
     * @return
     */
    @Override
    protected StringBuilder encodeShef(StringBuilder buffer, ObsCommon report,
            Headers headers) {
        int place = 0;
        try {

            Calendar c = WMOTimeParser.getSystemCalendar((String) headers
                    .get(DecoderTools.INGEST_FILE_NAME));

            StringBuilder lineHdr = new StringBuilder();
            lineHdr.append(SHEF_A_RECORD);
            if (report.getCorIndicator() != null) {
                lineHdr.append(SHEF_REVISION);
            }

            // ***********
            lineHdr.append(" ");
            place = 1;

            String id = report.getStationId();
            if (options.isOptCheckAliasId()) {
                id = options.checkAlias(id);
            }
            if (options.isOptStripICAO()) {
                id = id.substring(1);
            }
            lineHdr.append(id);

            lineHdr.append(" : SYNOPTIC : ");

            c = report.getTimeObs();
            place = 2;
            if (options.isOptCentury()) {
                lineHdr.append(String.format(SHEF_OBS_DATEY2K_FMT, c));
            } else {
                lineHdr.append(String.format(SHEF_OBS_DATE_FMT, c));
            }
            lineHdr.append(String.format(SHEF_OBS_TIME_FMT, c));
            if (options.isOptCentury()) {
                lineHdr.append(String.format(SHEF_OBS_BASISTIMEY2K_FMT, c));
            } else {
                lineHdr.append(String.format(SHEF_OBS_BASISTIME_FMT, c));
            }

            String reportText = "";

            for (SHEF_Obs_Codes<ObsCommon> value : SHEF_SM_Codes.values()) {
                try {
                    StringBuilder lineData = new StringBuilder();
                    place = 5;
                    value.format(lineData, report, reportText, options);
                    place = 6;
                    if (lineData.length() > 0) {
                        startMessageLine(buffer).append(lineHdr);
                        buffer.append(SHEF_SEPARATOR).append(lineData);
                    }
                } catch (RuntimeException e) {
                    logger.error("Error in encode:place " + place, e);
                }
            }
        } catch (Exception e) {
            logger.error("Error in encode:place " + place, e);
        }
        return buffer;
    }

    /**
     * Should this stations' data be encoded?
     * 
     * @param stationId
     *            A five digit WMO station identifier.
     * @return Should this stations' data be encoded? ======= Should this
     *         reports data be encoded?
     * 
     * @param report
     *            Data to be used to determine encoding status.
     * @return Should this reports data be encoded?
     */
    @Override
    protected boolean encodeThisStation(ObsCommon report) {
        boolean encode = false;
        if (report != null) {
            encode = (report.getReportType() == IDecoderConstants.SYNOPTIC_FIXED_LAND);
        }
        if (encode) {
            if (options.isOptCheckAliasId()) {
                String id = options.checkAlias(report.getStationId());
                encode = (id != null);
            }
        }
        return encode;
    }

}
