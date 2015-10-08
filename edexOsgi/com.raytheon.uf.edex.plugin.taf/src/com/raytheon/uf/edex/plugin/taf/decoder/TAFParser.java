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

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.taf.TAFParts;
import com.raytheon.uf.common.dataplugin.taf.TafConstants;
import com.raytheon.uf.common.dataplugin.taf.TafPeriod;
import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * The TAF parser accepts a potential TAF report and attempts to parse and
 * decode various information from that report.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20080424     1001        jkorman     Initial implementation.
 * 9/4/2008     1444        grichard    Import constants from TafConstants class.
 * Oct 21, 2008       1515  jkorman     Added 30 Hour capability changes.
 * Feb 27, 2013 1638        mschenke    Moved ObStationDao to edex pointdata plugin
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * May 15, 2014 3002        bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 *                                      Refactored Strings to Patterns in TafConstants.
 * Apr 01, 2015 3722        rjpeter     Updated amd/corindicator to boolean flags.
 * Sep 24, 2015 4890        rferrel     Remove ChangeGroup and code cleanup.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TAFParser {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    TafPeriod validPeriod = null;

    private boolean isCOR = false;

    private boolean isAMD = false;

    private TafRecord record = null;

    /**
     * 
     * @param tafParts
     * @param header
     * @throws DecoderException
     */
    public TAFParser(TAFParts tafParts, WMOHeader header)
            throws DecoderException {
        internalParse(tafParts, header);
    }

    /**
     * 
     * @return
     */
    public TafRecord getDecodedRecord() {
        return record;
    }

    /**
     * 
     * @param tafParts
     * @param header
     * @throws DecoderException
     */
    private void internalParse(TAFParts tafParts, WMOHeader header)
            throws DecoderException {

        TAFChangeGroupFactory fact = new TAFChangeGroupFactory();
        record = fact.getTafRecord(header, tafParts);

        String s = header.getYYGGgg();
        Calendar bulletinTime = transformDate(s, header);
        if (bulletinTime != null) {
            record.setBulletin_time(bulletinTime.getTime());
        }

        record.setTafText(tafParts.getTafHeader()
                + formatTAF(tafParts.getTafBody()));
        record.setWmoHeader(header.getWmoHeader());

        if (isAMD) {
            record.setAmdIndicator(true);
        }
        if (isCOR) {
            record.setCorIndicator(true);
        }

        ObStation location = null;
        try {
            location = new ObStationDao().queryByIcao(record.getStationId());
        } catch (DataAccessLayerException e) {
            logger.info("Error querying for ICAO [" + record.getStationId()
                    + "]");
        }

        if (location == null) {
            logger.info("Station id not found [" + record.getStationId() + "]");
            record = null;
        } else {
            record.setLocation(location);
        }
    }

    /**
     * Given a string in the form of DDHHMMZ, convert that string to a calendar
     * referenced to the current system calendar.
     * 
     * @param issueDateString
     *            Date/Time string to convert.
     * @return The created calendar.
     */
    private Calendar transformDate(String issueDateString, WMOHeader header) {

        Calendar tDate = TimeUtil.newGmtCalendar(header.getYear(),
                header.getMonth(), header.getDay());

        int maxDay = tDate.getActualMaximum(Calendar.DAY_OF_MONTH);

        int currDay = tDate.get(Calendar.DAY_OF_MONTH);

        int day = Integer.parseInt(issueDateString.substring(0, 2).trim());

        int dayDelta = currDay - day;

        if (dayDelta < 0) {
            if ((maxDay + dayDelta) == 1) {
                tDate.add(Calendar.DAY_OF_MONTH, -1);
            } else {
                tDate.add(Calendar.DAY_OF_MONTH, -1);
                tDate.set(Calendar.DAY_OF_MONTH, day);
            }
            logger.debug(" Less " + TafPeriod.formatDate(tDate));
        } else if (dayDelta > 0) {
            if ((maxDay - dayDelta) == 1) {
                tDate.add(Calendar.DAY_OF_MONTH, 1);
            } else {
                tDate.set(Calendar.DAY_OF_MONTH, currDay);
            }
            logger.debug(" Greater " + TafPeriod.formatDate(tDate));
        }

        int hour = Integer.parseInt(issueDateString.substring(2, 4).trim());
        int minute = Integer.parseInt(issueDateString.substring(4, 6).trim());

        tDate.set(Calendar.HOUR_OF_DAY, hour);
        tDate.set(Calendar.MINUTE, minute);
        tDate.set(Calendar.SECOND, 0);

        return tDate;
    }

    /**
     * 
     * @param groupValue
     * @return
     */
    public static boolean isChangeGroup(String groupValue) {

        boolean isTemp = TafConstants.CG_FM.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_BECMG.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_INITIAL.equals(groupValue);
        return isTemp;
    }

    /**
     * 
     * @param groupValue
     * @return
     */
    public static boolean isTempGroup(String groupValue) {

        boolean isTemp = TafConstants.CG_TEMPO.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_PROB_TEMPO.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_PROB.equals(groupValue);
        return isTemp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Integer cvtInt(String value) {
        int val = 0;
        for (int i = 0; i < value.length(); i++) {
            val *= 10;
            int v = Integer.parseInt(value.substring(i, i + 1));
            val += v;
        }
        return val;
    }

    private static String formatTAF(String taf) {
        StringBuilder sb = new StringBuilder(taf);
        int n = 0;
        String[] find = { "\r " + TafConstants.CG_FM,
                "\r " + TafConstants.CG_BECMG, "\r " + TafConstants.CG_TEMPO,
                "\r " + TafConstants.CG_PROB, };
        String[] replace = { "\r     " + TafConstants.CG_FM,
                "\r     " + TafConstants.CG_BECMG,
                "\r     " + TafConstants.CG_TEMPO,
                "\r     " + TafConstants.CG_PROB, };
        for (int i = 0; i < find.length; i++) {
            while ((n = sb.indexOf(find[i])) >= 0) {
                sb.replace(n, n + find[i].length(), replace[i]);
            }
        }
        Pattern p = Pattern.compile("\\r [^ ]");
        Matcher m = p.matcher(sb);
        while (m.find()) {
            sb.replace(m.start(), m.end() - 1, "\r      ");
            m = p.matcher(sb);
        }
        return sb.toString();
    }
}
