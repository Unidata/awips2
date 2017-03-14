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
package com.raytheon.uf.edex.metartohmdb.dao;

import java.util.Calendar;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import org.hibernate.Query;
import org.hibernate.Session;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Dao for HMDBReport.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009            jkorman     Initial creation
 * Sep 18, 2014 #3627      mapeters    Updated deprecated {@link TimeTools} usage.
 * Jun 18, 2015 4500       rjpeter     Fix SQL Injection concern.
 * Oct 30, 2015 5035       bclement    added getInsertStatement
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class HMDBRptDao extends CoreDao {

    public static final String REPORT_TABLE_NAME = "rpt";

    private static final Object LOCK = new Object();

    public HMDBRptDao() {
        super(DaoConfig.forClass("hmdb", HMDBReport.class));
    }

    /**
     * 
     * @param report
     * @return
     */
    public boolean storeToTable(HMDBReport report) {
        boolean status = true;
        synchronized (LOCK) {
            Map<String, Object> parameters = report.getColumnValues();
            String sql = getInsertStatement(REPORT_TABLE_NAME,
                    parameters.keySet());
            if (logger.isPriorityEnabled(Priority.DEBUG)) {
                /* avoid formatting parameters if not in debug */
                logger.debug("SQL = " + sql + " PARAMETERS = " + parameters);
            }
            try {
                status = (executeSQLUpdate(sql, parameters) == 1);
            } catch (Exception e) {
                logger.error("SQL = " + sql + " PARAMETERS = " + parameters);
                logger.error("Error writing to rpt table", e);
            }
        }
        return status;
    }

    /**
     * Create a parameterized insert statement that can be passed to
     * {@link CoreDao#executeSQLUpdate(String, Map)}
     * 
     * @param table
     * @param columns
     * @return
     */
    private static String getInsertStatement(String table,
            Collection<String> columns) {
        StringBuilder sb = new StringBuilder();
        sb.append("INSERT INTO ").append(table);
        sb.append(" (");
        if (!columns.isEmpty()) {
            Iterator<String> iter = columns.iterator();
            sb.append(iter.next());
            while (iter.hasNext()) {
                sb.append(',').append(iter.next());
            }
        }
        sb.append(") VALUES (");
        if (!columns.isEmpty()) {
            Iterator<String> iter = columns.iterator();
            sb.append(':').append(iter.next());
            while (iter.hasNext()) {
                sb.append(",:").append(iter.next());
            }
        }
        sb.append(");");
        return sb.toString();
    }

    /**
     * 
     * @return
     */
    public boolean purgeTable(final int purgeHours) {
        boolean status = true;
        final StringBuilder queryString = new StringBuilder();
        try {
            txTemplate.execute(new TransactionCallback<Integer>() {
                @Override
                public Integer doInTransaction(TransactionStatus status) {
                    Calendar c = TimeUtil.newGmtCalendar();
                    c.add(Calendar.HOUR_OF_DAY, -purgeHours);

                    Session sess = getCurrentSession();
                    Query query = sess
                            .createSQLQuery("delete from rpt where nominal < :nominal");
                    query.setCalendar("nominal", c);
                    queryString.append(query.getQueryString());
                    return query.executeUpdate();
                }
            });
        } catch (Exception e) {
            logger.error("Purge query = " + queryString);
            logger.error("Error in purging hmdb.rpt", e);
            status = false;
        }
        return status;
    }

}
