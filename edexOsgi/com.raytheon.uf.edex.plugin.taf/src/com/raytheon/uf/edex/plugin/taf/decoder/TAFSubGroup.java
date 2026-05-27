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
package com.raytheon.uf.edex.plugin.taf.decoder;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.taf.TafConstants;
import com.raytheon.uf.common.dataplugin.taf.TafPeriod;

/**
 * A TAF's subgroup header and body information.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2008       1515 jkorman     Initial implementation to
 *                                     add 30 Hour tafs.
 * May 15, 2014       3002 bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 * Sep 24, 2015       4890 rferrel     Remove ChangeGroup and code cleanup.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

class TAFSubGroup {

    private static final Pattern PAT_PROB = Pattern
            .compile(TAFChangeGroupFactory.PROB);

    private static final Pattern PAT_TEMPO = Pattern
            .compile(TAFChangeGroupFactory.TEMPO);

    private static final Pattern PAT_PROBTEMPO = Pattern
            .compile(TAFChangeGroupFactory.PROBTEMPO);

    private static final Pattern PAT_BECMG = Pattern
            .compile(TAFChangeGroupFactory.BECMG);

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final Pattern PAT_FM = Pattern
            .compile(TAFChangeGroupFactory.FM);

    private Integer startDay = null;

    private Integer startHour = null;

    private Integer startMin = 0;

    private Integer stopDay = null;

    private Integer stopHour = null;

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

        Matcher m = PAT_PROBTEMPO.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = TafConstants.CG_PROB_TEMPO;

            startDay = TAFParser.cvtInt(changeGroupHdr.substring(13, 15));
            startHour = TAFParser.cvtInt(changeGroupHdr.substring(15, 17));
            stopDay = TAFParser.cvtInt(changeGroupHdr.substring(18, 20));
            stopHour = TAFParser.cvtInt(changeGroupHdr.substring(20, 22));

            return;
        }
        m = PAT_PROB.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = TafConstants.CG_PROB;

            startDay = TAFParser.cvtInt(changeGroupHdr.substring(7, 9));
            startHour = TAFParser.cvtInt(changeGroupHdr.substring(9, 11));
            stopDay = TAFParser.cvtInt(changeGroupHdr.substring(12, 14));
            stopHour = TAFParser.cvtInt(changeGroupHdr.substring(14, 16));

            return;
        }
        m = PAT_TEMPO.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = TafConstants.CG_TEMPO;

            startDay = TAFParser.cvtInt(changeGroupHdr.substring(6, 8));
            startHour = TAFParser.cvtInt(changeGroupHdr.substring(8, 10));
            stopDay = TAFParser.cvtInt(changeGroupHdr.substring(11, 13));
            stopHour = TAFParser.cvtInt(changeGroupHdr.substring(13, 15));

            return;
        }
        m = PAT_BECMG.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = TafConstants.CG_BECMG;
            startDay = TAFParser.cvtInt(changeGroupHdr.substring(6, 8));
            startHour = TAFParser.cvtInt(changeGroupHdr.substring(8, 10));
            stopDay = TAFParser.cvtInt(changeGroupHdr.substring(11, 13));
            stopHour = TAFParser.cvtInt(changeGroupHdr.substring(13, 15));

            return;
        }
        m = PAT_FM.matcher(changeGroupHdr);
        if (m.find()) {
            changeGroupHeader = TafConstants.CG_FM;

            startDay = TAFParser.cvtInt(changeGroupHdr.substring(2, 4));
            startHour = TAFParser.cvtInt(changeGroupHdr.substring(4, 6));
            startMin = TAFParser.cvtInt(changeGroupHdr.substring(6, 8));

            return;
        }

        changeGroupHeader = TafConstants.CG_INITIAL;
        extension = changeGroupHdr;
        int pos = changeGroupHdr.length() - "DDMM/DDMM".length();

        startDay = TAFParser.cvtInt(changeGroupHdr.substring(pos, pos + 2));
        pos += 2;
        startHour = TAFParser.cvtInt(changeGroupHdr.substring(pos, pos + 2));
        pos += 3;
        stopDay = TAFParser.cvtInt(changeGroupHdr.substring(pos, pos + 2));
        pos += 2;
        stopHour = TAFParser.cvtInt(changeGroupHdr.substring(pos, pos + 2));
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
     * Determine the period for the subgroup.
     * 
     * @param issueTime
     *            - - Use to determine period's month and year
     * @return period - null when unable to determine the type of change group
     *         header.
     */
    public TafPeriod createPeriod(TafPeriod issueTime) {
        TafPeriod period = null;

        switch (changeGroupHeader) {
        case TafConstants.CG_FM:
            period = TafPeriod.determineChangeGroupPeriodDDhhmm(
                    startDay.intValue(), startHour.intValue(),
                    startMin.intValue(), issueTime);
            break;
        case TafConstants.CG_BECMG:
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(
                    startDay.intValue(), startHour.intValue(),
                    stopDay.intValue(), stopHour.intValue(), issueTime, true);
            break;

        // Same period determination.
        case TafConstants.CG_PROB:
        case TafConstants.CG_PROB_TEMPO:
        case TafConstants.CG_TEMPO:
        case "INITIAL":
            period = TafPeriod.determineChangeGroupPeriodDDhhDDhh(
                    startDay.intValue(), startHour.intValue(),
                    stopDay.intValue(), stopHour.intValue(), issueTime, false);
            break;
        default:
            logger.error("Unknown SubGroup value " + changeGroupHeader);
        }

        return period;
    }
}
