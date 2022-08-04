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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.gfe.smartinit.SmartInitRecordPK.State;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * SmartInit Transactions
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 12, 2010           njensen   Initial creation
 * May 20, 2014  3069     randerso  Added validTime to sort order when choosing
 *                                  next smartInit to run
 * Dec 15, 2015  5166     kbisanz   Update logging to use SLF4J
 * Feb 20, 2018  6928     randerso  Only delay live smartinits
 *
 * </pre>
 *
 * @author njensen
 */

public class SmartInitTransactions {

    protected static final String TASK_NAME = "GfeSmartInit";

    protected static transient Logger logger = LoggerFactory
            .getLogger(SmartInitTransactions.class);

    /**
     * Get next SmartInit to run
     *
     * @param pendingInitMinTimeMillis
     *            time to delay live SmartInits before running
     * @param runningInitTimeOutMillis
     *            time to wait for running inits before timing out
     * @return the next SmartInit to run if any
     */
    public static SmartInitRecord getSmartInitToRun(
            long pendingInitMinTimeMillis, long runningInitTimeOutMillis) {
        ClusterTask ct = ClusterLockUtils.lock("GfeSmartInit", "RunCheck",
                30 * TimeUtil.MILLIS_PER_SECOND, true);
        if (!ct.getLockState().equals(LockState.SUCCESSFUL)) {
            return null;
        }

        Session sess = null;
        Transaction trans = null;

        try {
            CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
            sess = dao.getSessionFactory().openSession();
            trans = sess.beginTransaction();

            // check the currently running SmartInits
            Criteria runningCrit = sess.createCriteria(SmartInitRecord.class);
            Criterion stateRunningCrit = Restrictions.eq("id.state",
                    State.RUNNING);
            runningCrit.add(stateRunningCrit);
            runningCrit.addOrder(Order.asc("priority"))
                    .addOrder(Order.asc("insertTime"));

            @SuppressWarnings("unchecked")
            List<SmartInitRecord> currentlyRunning = runningCrit.list();

            if (!currentlyRunning.isEmpty()) {
                // see if any have timed out
                long timeOutCheck = System.currentTimeMillis()
                        - runningInitTimeOutMillis;
                for (SmartInitRecord record : currentlyRunning) {
                    if (record.getInsertTime().getTime() < timeOutCheck) {
                        // lock the row to ensure no one else will run the
                        // upgrade/delete the row
                        record = (SmartInitRecord) sess.get(
                                SmartInitRecord.class, record.getId(),
                                LockOptions.UPGRADE);
                        // double check to make sure another process hasn't
                        // already grabbed it and the run didn't finish
                        if ((record != null) && (record.getInsertTime()
                                .getTime() < timeOutCheck)) {
                            logger.info("Running smartInit " + record.getId()
                                    + " timed out.  Rerunning smartInit.");
                            record.setInsertTime(
                                    new Date(System.currentTimeMillis()));
                            sess.update(record);
                            trans.commit();
                            return record;
                        }
                    }
                }
            }

            // query the pending table for available inits
            Criteria pendingCrit = sess.createCriteria(SmartInitRecord.class);
            pendingCrit.addOrder(Order.asc("priority"))
                    .addOrder(Order.asc("insertTime"))
                    .addOrder(Order.asc("id.validTime"));
            long pendingTimeRest = System.currentTimeMillis()
                    - pendingInitMinTimeMillis;

            Criterion baseCrit = Restrictions.eq("id.state", State.PENDING);
            baseCrit = Restrictions.and(baseCrit, Restrictions.or(
                    Restrictions.ne("priority",
                            SmartInitRecord.LIVE_SMART_INIT_PRIORITY),
                    Restrictions.le("insertTime", new Date(pendingTimeRest))));
            if (!currentlyRunning.isEmpty()) {
                // exclude the running inits
                Collection<String> runningInits = new ArrayList<>(
                        currentlyRunning.size());
                for (SmartInitRecord record : currentlyRunning) {
                    runningInits.add(record.getId().getInitName());
                }

                baseCrit = Restrictions.and(baseCrit, Restrictions
                        .not(Restrictions.in("id.initName", runningInits)));
            }

            pendingCrit.add(baseCrit);

            // possible inits to run
            @SuppressWarnings("unchecked")
            List<SmartInitRecord> pendingRecords = pendingCrit.list();

            for (SmartInitRecord record : pendingRecords) {
                // lock the init
                record = (SmartInitRecord) sess.get(SmartInitRecord.class,
                        record.getId(), LockOptions.UPGRADE);

                // double check its still valid
                if ((record != null) && (record
                        .getPriority() != SmartInitRecord.LIVE_SMART_INIT_PRIORITY
                        || record.getInsertTime()
                                .getTime() <= pendingTimeRest)) {
                    sess.delete(record);
                    // can we update primary key in place?? or do we need to
                    // delete then add
                    record = (SmartInitRecord) record.clone();
                    record.getId().setState(State.RUNNING);
                    record.setInsertTime(new Date());
                    sess.save(record);
                    trans.commit();
                    return record;
                }
            }
            trans.commit();
        } catch (Throwable t) {
            logger.error("Querying for available SmartInits failed", t);
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (HibernateException e) {
                    logger.warn(
                            "Unable to rollback available SmartInit transaction",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (HibernateException e) {
                    logger.warn("Unable to close available SmartInit session",
                            e);
                }
            }
            ClusterLockUtils.unlock(ct, false);
        }

        return null;
    }

    /**
     * Remove a SmartInit record from the database
     *
     * @param record
     *            the record to remove
     *
     */
    public static void removeSmartInit(SmartInitRecord record) {
        Session sess = null;
        Transaction tx = null;

        try {
            CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
            sess = dao.getSessionFactory().openSession();
            tx = sess.beginTransaction();

            // lock the row to ensure no one else will run the
            // upgrade/delete the row
            record = (SmartInitRecord) sess.get(SmartInitRecord.class,
                    record.getId(), LockOptions.UPGRADE);

            if (record != null) {
                sess.delete(record);
            }
            tx.commit();
        } catch (Throwable t) {
            logger.error("Unable to remove SmartInit", t);
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    logger.warn("Unable to rollback SmartInit remove session",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (HibernateException e) {
                    logger.warn("Unable to close SmartInit remove session", e);
                }
            }
        }
    }
}
