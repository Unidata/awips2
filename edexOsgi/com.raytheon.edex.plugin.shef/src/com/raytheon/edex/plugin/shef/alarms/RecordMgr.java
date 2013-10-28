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
package com.raytheon.edex.plugin.shef.alarms;

import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.ALARM_CATEGSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.ALERT_CATEGSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.DIFF_CHECKSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.LOWER_CHECKSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.ROC_CHECKSTR;
import static com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.UPPER_CHECKSTR;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
 * June 15, 2011    9377     jnjanga     Initial creation
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

    private static Log log = LogFactory
            .getLog(RecordMgr.class);

    private static ReportOptions options = null;

    public static AlertalarmRecord getAlarmData(ReportOptions opt) {
        options = opt;
        StringBuilder query = new StringBuilder(
                "select aav.lid, "
                        + "aav.pe, aav.dur, "
                        + "aav.ts, aav.extremum, "
                        + "aav.probability, aav.validtime, aav.basistime, "
                        + "aav.aa_categ, aav.aa_check, "
                        + "aav.value, aav.suppl_value, "
                        + "aav.shef_qual_code, aav.quality_code, aav.revision, "
                        + "aav.product_id, aav.producttime, aav.postingtime, aav.action_time, "
                        + "location.name from location, alertalarmval aav where location.lid = aav.lid");

        // Build 'where' clause according to report mode
        // if getting only unreported data, let the query filter out the
        // reported data
        query.append(whereSubClauseFor(MODE))
                .append(whereSubClauseFor(TYPE_SRC))
                .append(whereSubClauseFor(AA_CAT))
                .append(whereSubClauseFor(AA_CHCK))
                .append(whereSubClauseFor(PEFILTER))
                .append(" AND (aav.ts NOT LIKE 'F%' OR aav.validtime >= current_timestamp) ")
                .append(" ORDER BY aav.lid ASC, aav.pe, aav.ts, aav.aa_check, aav.validtime DESC ");

        log.info("Query for getting alertalarmval data :" + query.toString());

        Object[] aaData = null;
        CoreDao dao = null;
        AlertalarmRecord aaRecord = null;

        // Get the data
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            aaData = dao.executeSQLQuery(query.toString());
            if (aaData != null && aaData.length > 0) {
                aaRecord = AlertalarmRecord.newInstance();
                for (int i = 0; i < aaData.length; i++) {
                    Object[] aaRow = (Object[]) aaData[i];
                    aaRecord.put(aaRow);
                }
            }
            
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(" - PostgresSQL error retrieving from alertalarmval ", e);
            System.exit(0);
        }

        return aaRecord;
    }

    private static String whereSubClauseFor(int userSelection) {
        switch (userSelection) {
        case MODE:
            return modeSubClause();
        case TYPE_SRC:
            return typeSrcSubClause();
        case AA_CAT:
            return aaCatSubClause();
        case AA_CHCK:
            return aaCheckSubClause();
        case PEFILTER:
            return peFilterSubClause();
        default:
            return null;
        }

    }

    /**
     * Adjust the query to any PE Filter
     * 
     * @return
     */
    private static String peFilterSubClause() {
        String pe = options.getPEfilter();
        return pe == null ? "" : " AND pe = " + pe;
    }

    private static String modeSubClause() {
        if (options.getMode() == ReportMode.UNREPORTED)
            return " AND action_time IS NULL AND ";
        else
            return " ";
    }

    private static String typeSrcSubClause() {
        String flags = options.getFilter();
        if(flags==null)
            return " AND aav.ts like '%'";
        
        if (flags.contains("O") && !flags.contains("F"))
            return " AND (aav.ts like 'R%' or aav.ts like 'P%')";
        else if (!flags.contains("O") && flags.contains("F"))
            return " AND (aav.ts like 'F%' or aav.ts like 'C%')";
        else
            return " AND aav.ts like '%'";
    }

    /**
     * append the where clause based on the alert/alarm category field
     * 
     * @return
     */
    private static String aaCatSubClause() {
        String flags = options.getFilter();
        if(flags == null)
            return " ";
        
        if (flags.contains("T") && !flags.contains("M"))
            return " AND aav.aa_categ = " + ALERT_CATEGSTR;
        else if (!flags.contains("T") && flags.contains("M"))
            return " AND aav.aa_categ = " + ALARM_CATEGSTR;
        else
            return " ";
    }

    /**
     * append the where clause based on the alert/alarm check field
     * 
     * @return
     */
    private static String aaCheckSubClause() {
        String subClause = " AND aa_check in (";
        String flags = options.getFilter();
        if(flags == null)
            return " ";
        
        boolean rocFlag = flags.contains("R");
        boolean lowFlag = flags.contains("L");
        boolean upFlag = flags.contains("U");
        boolean diffFlag = flags.contains("D");

        if (!rocFlag && !lowFlag && !upFlag && !diffFlag)
            return " ";
        else {
            boolean init = true;

            char[] checks = { 'R', 'L', 'U', 'D' };
            for (char c : checks) {
                switch (c) {
                case 'U':
                    if (upFlag)
                        subClause
                                .concat(aaCheckSubClause(init, UPPER_CHECKSTR));
                    break;
                case 'L':
                    if (lowFlag)
                        subClause
                                .concat(aaCheckSubClause(init, LOWER_CHECKSTR));
                    break;
                case 'R':
                    if (rocFlag)
                        subClause.concat(aaCheckSubClause(init, ROC_CHECKSTR));
                    break;
                case 'D':
                    if (diffFlag)
                        subClause.concat(aaCheckSubClause(init, DIFF_CHECKSTR));
                    break;
                default:
                    break;
                }
            }
        }
        return subClause.concat(" ) ");
    }

    private static String aaCheckSubClause(boolean initialEntry, String checkStr) {
        if (initialEntry) {
            initialEntry = !initialEntry;
            return checkStr;
        } else
            return "," + checkStr;
    }

}