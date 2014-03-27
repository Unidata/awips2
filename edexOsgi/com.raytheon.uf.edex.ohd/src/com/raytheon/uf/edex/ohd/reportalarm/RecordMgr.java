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
package com.raytheon.uf.edex.ohd.reportalarm;

import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.ALARM_CATEGSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.ALERT_CATEGSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.DIFF_CHECKSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.LOWER_CHECKSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.ROC_CHECKSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.UPPER_CHECKSTR;

import java.util.EnumSet;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Reads Alertalarmval table and creates an organized record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2011  9377      jnjanga      Initial creation
 * Sep 05, 2013  16549     wkwock       Fix the query
 * Feb 13, 2014  #2783     dgilling     Refactored to support running as part
 *                                      of an EDEX service.
 * 
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

class RecordMgr {

    static final int MODE = 101;

    static final int TYPE_SRC = 102;

    static final int AA_CAT = 103;

    static final int AA_CHCK = 104;

    static final int PEFILTER = 105;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RecordMgr.class);

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private RecordMgr() {
        throw new AssertionError();
    }

    public static AlertalarmRecord getAlarmData(ReportOptions opt) {
        StringBuilder query = new StringBuilder("select aav.lid, ")
                .append("aav.pe, aav.dur, ")
                .append("aav.ts, aav.extremum, ")
                .append("aav.probability, aav.validtime, aav.basistime, ")
                .append("aav.aa_categ, aav.aa_check, ")
                .append("aav.value, aav.suppl_value, ")
                .append("aav.shef_qual_code, aav.quality_code, aav.revision, ")
                .append("aav.product_id, aav.producttime, aav.postingtime, aav.action_time, ")
                .append("location.name from location, alertalarmval aav where location.lid = aav.lid");

        // Build 'where' clause according to report mode
        // if getting only unreported data, let the query filter out the
        // reported data
        query.append(whereSubClauseFor(MODE, opt))
                .append(whereSubClauseFor(TYPE_SRC, opt))
                .append(whereSubClauseFor(AA_CAT, opt))
                .append(whereSubClauseFor(AA_CHCK, opt))
                .append(whereSubClauseFor(PEFILTER, opt))
                .append(" AND (aav.ts NOT LIKE 'F%' OR aav.validtime >= current_timestamp) ")
                .append(" ORDER BY aav.lid ASC, aav.pe, aav.ts, aav.aa_check, aav.validtime DESC ");

        statusHandler.info("Query for getting alertalarmval data :"
                + query.toString());

        // Get the data
        AlertalarmRecord aaRecord = null;
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
        Object[] aaData = dao.executeSQLQuery(query.toString());
        if (aaData != null && aaData.length > 0) {
            aaRecord = AlertalarmRecord.newInstance();
            for (int i = 0; i < aaData.length; i++) {
                Object[] aaRow = (Object[]) aaData[i];
                aaRecord.put(aaRow);
            }
        }

        return aaRecord;
    }

    private static String whereSubClauseFor(int userSelection,
            ReportOptions options) {
        switch (userSelection) {
        case MODE:
            return modeSubClause(options.getMode());
        case TYPE_SRC:
            return typeSrcSubClause(options.getFilter());
        case AA_CAT:
            return aaCatSubClause(options.getFilter());
        case AA_CHCK:
            return aaCheckSubClause(options.getFilter());
        case PEFILTER:
            return peFilterSubClause(options.getPEfilter());
        default:
            return null;
        }

    }

    /**
     * Adjust the query to any PE Filter
     * 
     * @return
     */
    private static String peFilterSubClause(String pe) {
        return pe == null ? "" : " AND pe = " + pe;
    }

    private static String modeSubClause(ReportMode mode) {
        if (mode == ReportMode.UNREPORTED) {
            return " AND action_time IS NULL ";
        } else {
            return " ";
        }
    }

    private static String typeSrcSubClause(EnumSet<FilterOption> flags) {
        if (flags == null) {
            return " AND aav.ts like '%'";
        }

        if (flags.contains(FilterOption.OBSERVED)
                && !flags.contains(FilterOption.FORECAST)) {
            return " AND (aav.ts like 'R%' or aav.ts like 'P%')";
        } else if (!flags.contains(FilterOption.OBSERVED)
                && flags.contains(FilterOption.FORECAST)) {
            return " AND (aav.ts like 'F%' or aav.ts like 'C%')";
        } else {
            return " AND aav.ts like '%'";
        }
    }

    /**
     * append the where clause based on the alert/alarm category field
     * 
     * @return
     */
    private static String aaCatSubClause(EnumSet<FilterOption> flags) {
        if (flags == null) {
            return " ";
        }

        if (flags.contains(FilterOption.ALERTS)
                && !flags.contains(FilterOption.ALARMS)) {
            return " AND aav.aa_categ = " + ALERT_CATEGSTR;
        } else if (!flags.contains(FilterOption.ALERTS)
                && flags.contains(FilterOption.ALARMS)) {
            return " AND aav.aa_categ = " + ALARM_CATEGSTR;
        } else {
            return " ";
        }
    }

    /**
     * append the where clause based on the alert/alarm check field
     * 
     * @return
     */
    private static String aaCheckSubClause(EnumSet<FilterOption> flags) {
        String subClause = " AND aa_check in (";
        if (flags == null) {
            return " ";
        }

        boolean rocFlag = flags.contains(FilterOption.RATE_OF_CHANGE);
        boolean lowFlag = flags.contains(FilterOption.LOWER_LIMIT);
        boolean upFlag = flags.contains(FilterOption.UPPER_LIMIT);
        boolean diffFlag = flags.contains(FilterOption.DIFF_LIMIT);

        if (!rocFlag && !lowFlag && !upFlag && !diffFlag) {
            return " ";
        } else {
            boolean init = true;

            if (rocFlag) {
                subClause.concat(aaCheckSubClause(init, ROC_CHECKSTR));
            }

            if (lowFlag) {
                subClause.concat(aaCheckSubClause(init, LOWER_CHECKSTR));
            }

            if (upFlag) {
                subClause.concat(aaCheckSubClause(init, UPPER_CHECKSTR));
            }

            if (diffFlag) {
                subClause.concat(aaCheckSubClause(init, DIFF_CHECKSTR));
            }
        }

        return subClause.concat(" ) ");
    }

    private static String aaCheckSubClause(boolean initialEntry, String checkStr) {
        if (initialEntry) {
            initialEntry = !initialEntry;
            return checkStr;
        } else {
            return "," + checkStr;
        }
    }

}