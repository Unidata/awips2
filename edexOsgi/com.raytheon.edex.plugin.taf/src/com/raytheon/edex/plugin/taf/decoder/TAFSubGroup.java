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
package com.raytheon.edex.plugin.taf.decoder;

import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_BECMG;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_FM;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_INITIAL;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_PROB;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_PROB_TEMPO;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_TEMPO;
import static com.raytheon.edex.plugin.taf.decoder.TAFParser.cvtInt;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.taf.common.ChangeGroup;
import com.raytheon.edex.plugin.taf.common.TafPeriod;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2008       1515 jkorman     Initial implementation to
 *                                     add 30 Hour tafs.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

class TAFSubGroup {

    private static final Pattern PAT_VALID_TIME = Pattern
            .compile(TAFChangeGroupFactory.VALID_TIME);

    private static final Pattern PAT_PROB = Pattern
            .compile(TAFChangeGroupFactory.PROB);

    private static final Pattern PAT_TEMPO = Pattern
            .compile(TAFChangeGroupFactory.TEMPO);

    private static final Pattern PAT_PROBTEMPO = Pattern
            .compile(TAFChangeGroupFactory.PROBTEMPO);

    private static final Pattern PAT_BECMG = Pattern
            .compile(TAFChangeGroupFactory.BECMG);

    private final Log logger = LogFactory.getLog(getClass());

    private static final Pattern PAT_FM = Pattern
            .compile(TAFChangeGroupFactory.FM);

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

            prob = Integer.decode(changeGroupHdr.substring(4, 5));

            startDay = cvtInt(changeGroupHdr.substring(13, 15));
            startHour = cvtInt(changeGroupHdr.substring(15, 17));
            stopDay = cvtInt(changeGroupHdr.substring(18, 20));
            stopHour = cvtInt(changeGroupHdr.substring(20, 22));

            return;
        }
        m = PAT_PROB.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_PROB;

            prob = Integer.decode(changeGroupHdr.substring(4, 5));

            startDay = cvtInt(changeGroupHdr.substring(7, 9));
            startHour = cvtInt(changeGroupHdr.substring(9, 11));
            stopDay = cvtInt(changeGroupHdr.substring(12, 14));
            stopHour = cvtInt(changeGroupHdr.substring(14, 16));

            return;
        }
        m = PAT_TEMPO.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = CG_TEMPO;

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
    public ChangeGroup toChangeGroup(TafPeriod issueTime) {

        ChangeGroup chgGroup = null;

        TafPeriod period = null;
        if (CG_FM.equals(changeGroupHeader)) {
            period = TafPeriod.determineChangeGroupPeriodDDhhmm(startDay
                    .intValue(), startHour.intValue(), startMin.intValue(),
                    issueTime);
        } else if (CG_BECMG.equals(changeGroupHeader)) {
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, true);
        } else if (CG_PROB.equals(changeGroupHeader)) {
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else if (CG_PROB_TEMPO.equals(changeGroupHeader)) {
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else if (CG_TEMPO.equals(changeGroupHeader)) {
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else if ("INITIAL".equals(changeGroupHeader)) {
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(startDay
                    .intValue(), startHour.intValue(), stopDay.intValue(),
                    stopHour.intValue(), issueTime, false);
        } else {
            logger.error("Unknown SubGroup value " + changeGroupHeader);
        }
        if (period != null) {
            chgGroup = new ChangeGroup(changeGroupHeader, changeGroupBody,
                    period, period);
            chgGroup.setChangeGroup(changeGroupData + changeGroupBody);
            chgGroup.setProbability(prob);
        }
        return chgGroup;
    }

}
