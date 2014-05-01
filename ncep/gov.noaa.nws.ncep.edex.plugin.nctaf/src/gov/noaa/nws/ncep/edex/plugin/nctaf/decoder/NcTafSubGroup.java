/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.nctaf.decoder;

import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_BECMG;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_FM;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_INITIAL;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_PROB;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_PROB_TEMPO;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_TEMPO;
import static gov.noaa.nws.ncep.edex.plugin.nctaf.decoder.NcTafParser.cvtInt;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafChangeGroup;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafPeriod;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09/2011   458		   sgurung	   Initial Creation from Raytheon's taf plugin
 * 10/25/2011   		   sgurung	   Set probability=50 for TEMPO change group
 * 11/02/2011   		   sgurung	   Set probability * 10
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

class NcTafSubGroup {

    private static final Pattern PAT_VALID_TIME = Pattern
            .compile(NcTafChangeGroupFactory.VALID_TIME);

    private static final Pattern PAT_PROB = Pattern
            .compile(NcTafChangeGroupFactory.PROB);

    private static final Pattern PAT_TEMPO = Pattern
            .compile(NcTafChangeGroupFactory.TEMPO);

    private static final Pattern PAT_PROBTEMPO = Pattern
            .compile(NcTafChangeGroupFactory.PROBTEMPO);

    private static final Pattern PAT_BECMG = Pattern
            .compile(NcTafChangeGroupFactory.BECMG);

    private final Log logger = LogFactory.getLog(getClass());

    private static final Pattern PAT_FM = Pattern
            .compile(NcTafChangeGroupFactory.FM);

    private Integer prob = null;

    private Integer startDay = null;

    private Integer startHour = null;

    private Integer startMin = 0;

    private Integer stopDay = null;

    private Integer stopHour = null;

    private String changeGroupData = null;

    private String changeGroupHeader;

    private String changeGroupBody;

    // Hold the data for the initial group.
    private String extension;

    /**
     * @return the changeGroupHeader
     */
    public String getChangeGroupHeader() {
        return changeGroupHeader;
    }

    /**
     * @param changeGroupHdr
     *            the changeGroupHeader to set
     */
    public void setChangeGroupHeader(String changeGroupHdr) {
        changeGroupData = changeGroupHdr;

        Matcher m = PAT_PROBTEMPO.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_PROB_TEMPO;

            prob = Integer.decode(changeGroupHdr.substring(4, 5)) * 10;

            startDay = cvtInt(changeGroupHdr.substring(13, 15));
            startHour = cvtInt(changeGroupHdr.substring(15, 17));
            stopDay = cvtInt(changeGroupHdr.substring(18, 20));
            stopHour = cvtInt(changeGroupHdr.substring(20, 22));

            return;
        }
        m = PAT_PROB.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_PROB;

            prob = Integer.decode(changeGroupHdr.substring(4, 5)) * 10;

            startDay = cvtInt(changeGroupHdr.substring(7, 9));
            startHour = cvtInt(changeGroupHdr.substring(9, 11));
            stopDay = cvtInt(changeGroupHdr.substring(12, 14));
            stopHour = cvtInt(changeGroupHdr.substring(14, 16));

            return;
        }
        m = PAT_TEMPO.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_TEMPO;
            
            // probability equals 50 for TEMPO change group
            prob = 50;

            startDay = cvtInt(changeGroupHdr.substring(6, 8));
            startHour = cvtInt(changeGroupHdr.substring(8, 10));
            stopDay = cvtInt(changeGroupHdr.substring(11, 13));
            stopHour = cvtInt(changeGroupHdr.substring(13, 15));

            return;
        }
        m = PAT_BECMG.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_BECMG;
            startDay = cvtInt(changeGroupHdr.substring(6, 8));
            startHour = cvtInt(changeGroupHdr.substring(8, 10));
            stopDay = cvtInt(changeGroupHdr.substring(11, 13));
            stopHour = cvtInt(changeGroupHdr.substring(13, 15));

            return;
        }
        m = PAT_FM.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_FM;

            startDay = cvtInt(changeGroupHdr.substring(2, 4));
            startHour = cvtInt(changeGroupHdr.substring(4, 6));
            startMin = cvtInt(changeGroupHdr.substring(6, 8));

            return;
        }

        changeGroupHeader = CG_INITIAL;
        extension = changeGroupHdr;
        int pos = changeGroupHdr.length() - "DDMM/DDMM".length();

        startDay = cvtInt(changeGroupHdr.substring(pos, pos + 2));
        pos += 2;
        startHour = cvtInt(changeGroupHdr.substring(pos, pos + 2));
        pos += 3;
        stopDay = cvtInt(changeGroupHdr.substring(pos, pos + 2));
        pos += 2;
        stopHour = cvtInt(changeGroupHdr.substring(pos, pos + 2));
    }

    /**
     * @return the changeGroupBody
     */
    public String getChangeGroupBody() {
        return changeGroupBody;
    }

    /**
     * @param changeGroupBody
     *            the changeGroupBody to set
     */
    public void setChangeGroupBody(String changeGroupBody) {
        this.changeGroupBody = changeGroupBody;
    }

    /**
     * 
     * @return
     */
    public String getExtension() {
        return extension;
    }

    @Override
    public String toString() {
        return "{" + changeGroupHeader + "[" + changeGroupBody + "]}";
    }

    /**
     * Convert this SubGroup instance into a TAF ChangeGroup.
     * 
     * @param issueTime
     *            The time that the enclosing TAF was issued.
     * @return
     */
    public NcTafChangeGroup toChangeGroup(NcTafPeriod issueTime) {

        NcTafChangeGroup chgGroup = null;

        NcTafPeriod period = null;
        if (CG_FM.equals(changeGroupHeader)) {
            period = NcTafPeriod.determineChangeGroupPeriodDDhhmm(startDay
                    .intValue(), startHour.intValue(), startMin.intValue(),
                    issueTime);
        } else if (CG_BECMG.equals(changeGroupHeader)) {
            period = NcTafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, true);
        } else if (CG_PROB.equals(changeGroupHeader)) {
            period = NcTafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else if (CG_PROB_TEMPO.equals(changeGroupHeader)) {
            period = NcTafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else if (CG_TEMPO.equals(changeGroupHeader)) {
            period = NcTafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else if ("INITIAL".equals(changeGroupHeader)) {
            period = NcTafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else {
            logger.error("Unknown SubGroup value " + changeGroupHeader);
        }
        if (period != null) {
            chgGroup = new NcTafChangeGroup(changeGroupHeader, changeGroupBody,
                    period, period);
            chgGroup.setChangeGroup(changeGroupData + changeGroupBody);
            chgGroup.setProbability(prob);
        }
        return chgGroup;
    }

}
