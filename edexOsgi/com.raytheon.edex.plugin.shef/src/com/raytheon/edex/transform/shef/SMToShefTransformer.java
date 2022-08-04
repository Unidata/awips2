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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import javax.xml.transform.TransformerException;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.transform.shef.obs.MetarToShefConfigReader;
import com.raytheon.edex.transform.shef.obs.ObsToSHEFOptions;
import com.raytheon.edex.transform.shef.obs.SHEF_Obs_Codes;
import com.raytheon.edex.transform.shef.obs.SHEF_SM_Codes;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.ohd.AppsDefaults;
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
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Oct 29, 2008  1659     jkorman    Initial creation
 * Sep 18, 2012  1185     jkorman    Added save to archive capability.
 * May 14, 2014  2536     bclement   moved WMO Header to common, removed
 *                                   TimeTools usage
 * Jul 01, 2015  16903    lbousaidi  fixed WMO header in ingest log and
 *                                   product_id inserted into ihfs database
 * Oct 28, 2015  4783     bkowal     Allow {@link SynopticToShefRun}s to
 *                                   override the default configuration.
 * Dec 16, 2015  5166     kbisanz    Update logging to use SLF4J
 * Jun 01, 2018  6843     mduff      Inject AppsDefaults and ConfigReader, code
 *                                   clean up.
 * Jul 10, 2019  6843     randerso   Remove unnecessary AppsDefaultsWrapper
 * 
 * </pre>
 * 
 * @author jkorman
 */

public class SMToShefTransformer extends AbstractShefTransformer<ObsCommon> {

    private static final String WMO_HEADER_FMT = CRCRLF
            + "SRXX99 %4s %2$td%2$tH%2$tM";

    private static final int DT_SIZE = 6;

    private static final int CCC_SIZE = 6;

    private static final int MAX_LIST = 500;

    private static Map<Integer, SynopticToShefRun> matchMap = new HashMap<>();

    private final ObsToSHEFOptions defaultOptions;

    private Map<String, ObsToSHEFOptions> optionsCacheMap = new HashMap<>();

    private static final DateFormat dateFormat = new SimpleDateFormat("ddHHmm");
    static {
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * 
     */
    public SMToShefTransformer(String cmdLine, AppsDefaults appsDefaults,
            MetarToShefConfigReader configReader) {
        super(cmdLine, WMO_HEADER_FMT, appsDefaults, configReader);
        this.defaultOptions = this.options;
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

        ObsToSHEFOptions tmpOptions = null;
        SynopticToShefRun run = matchMap.get(report.getId());
        if (run == null) {
            tmpOptions = this.defaultOptions;
        } else {
            final String optionsKey = run.getConfigFileName();
            tmpOptions = this.optionsCacheMap.get(optionsKey);
            if (tmpOptions == null) {
                if (this.optionsCacheMap.size() > MAX_LIST) {
                    this.optionsCacheMap.clear();
                }
                /*
                 * {@link #metar2ShefOptions} contain the parameters that were
                 * used when constructing the default {@link ObsToSHEFOptions}
                 * options.
                 */
                if (run.getConfigFileName() == null) {
                    tmpOptions = new ObsToSHEFOptions(this.metar2ShefOptions,
                            true, configReader);
                } else {
                    tmpOptions = new ObsToSHEFOptions(run.getConfigFileName(),
                            this.metar2ShefOptions, true, configReader);
                }
                this.optionsCacheMap.put(optionsKey, tmpOptions);
            }
            matchMap.remove(report.getId());
        }
        this.options = tmpOptions;
        logger.info("Synoptic to SHEF for " + report.getStationId()
                + " use config file: " + options.getCfgFileName());

        // Transformed Synoptic PluginDataObject to SHEF
        byte[] result = null;
        try {
            // Currently returns false, so nothing is encoded at this time.
            if (encodeThisStation(report)) {
                // get the id and use the alias to change digits to letters.
                String stnId = report.getStationId();

                if (options.isOptCheckAliasId()) {
                    stnId = options.checkAlias(stnId);
                }
                // make header for ingest log file printout
                String YYGGgg = report.getWmoHeader().substring(12,
                        12 + DT_SIZE);
                String ccc = report.getWmoHeader().substring(6, 6 + CCC_SIZE);

                StringBuilder sb = makeSynHeader(openWMOMessage(200), stnId,
                        headers, YYGGgg);
                String fileName = makeSynHeader(new StringBuilder(20), stnId,
                        headers, YYGGgg).toString().trim().replace(' ', '_');

                startMessageLine(sb);

                if (ccc != null) {
                    if (ccc.length() > 3) {
                        ccc = ccc.substring(ccc.length() - 4).trim();
                    }
                }

                sb.append(ccc);
                sb.append(METAR_2_SHEF_NNN);
                if (stnId.length() == 4) {
                    sb.append(stnId.substring(1));
                } else if (stnId.length() == 3) {
                    sb.append(stnId);
                }

                startMessageLine(sb);
                startMessageLine(sb).append(
                        ": SHEF derived data created by SMToShefTransformer");
                startMessageLine(sb).append(": TRACEID = ");

                report.getWmoHeader();
                sb.append(report.getWmoHeader());

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
            logger.error(
                    "Error transforming input report to SHEF encoded report",
                    e);
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

            Calendar c = WMOTimeParser.getSystemCalendar(
                    (String) headers.get(DecoderTools.INGEST_FILE_NAME));

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
            encode = (report
                    .getReportType() == IDecoderConstants.SYNOPTIC_FIXED_LAND);
        }
        if (encode) {
            if (options.isOptCheckAliasId()) {
                String id = options.checkAlias(report.getStationId());
                encode = (id != null);
            }
        }
        return encode;
    }

    public static void setMatchMap(
            final Map<Integer, SynopticToShefRun> reportMatchMap) {
        if (matchMap.size() > MAX_LIST) {
            matchMap.clear();
        }
        matchMap.putAll(reportMatchMap);
    }
}
