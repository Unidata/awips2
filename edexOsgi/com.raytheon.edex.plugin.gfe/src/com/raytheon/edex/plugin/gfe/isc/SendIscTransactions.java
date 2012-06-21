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
package com.raytheon.edex.plugin.gfe.isc;

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

import com.raytheon.edex.plugin.gfe.isc.IscSendRecord.IscSendState;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Utility functions for IscSendJob and ISCSendQueue.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling     Initial creation
 * May 08, 2012  #600      dgilling     Refactor to match IscSendRecord
 *                                      changes.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SendIscTransactions {

    private static final transient IUFStatusHandler handler = UFStatus
            .getHandler(SendIscTransactions.class);

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     */
    private SendIscTransactions() {
        throw new AssertionError();
    }

    @SuppressWarnings("unchecked")
    public static IscSendRecord getNextSendJob(long runningTimeOutMillis) {
        Session sess = null;
        Transaction trans = null;

        try {
            CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
            sess = dao.getSessionFactory().openSession();
            trans = sess.beginTransaction();

            // check the currently send jobs
            Criteria runningCrit = sess.createCriteria(IscSendRecord.class);
            Criterion stateRunningCrit = Restrictions.eq("state",
                    IscSendState.RUNNING);
            runningCrit.add(stateRunningCrit);
            runningCrit.addOrder(Order.asc("insertTime"));
            List<IscSendRecord> currentlyRunning = runningCrit.list();

            if (!currentlyRunning.isEmpty()) {
                // see if any have timed out
                long timeOutCheck = System.currentTimeMillis()
                        - runningTimeOutMillis;
                for (IscSendRecord record : currentlyRunning) {
                    if (record.getInsertTime().getTime() < timeOutCheck) {
                        // lock the row to ensure no one else will run the
                        // upgrade/delete the row
                        record = (IscSendRecord) sess.get(IscSendRecord.class,
                                record.getKey(), LockOptions.UPGRADE);
                        // double check to make sure another process hasn't
                        // already grabbed it and the run didn't finish
                        if (record != null
                                && record.getInsertTime().getTime() < timeOutCheck) {
                            handler.info("Running IscSendJob "
                                    + record.getKey()
                                    + " timed out.  Rerunning IscSendJob.");
                            record.setInsertTime(new Date());
                            sess.update(record);
                            trans.commit();
                            return record;
                        }
                    }
                }
            }

            // query the pending table for available send jobs
            Criteria queuedCrit = sess.createCriteria(IscSendRecord.class);
            queuedCrit.addOrder(Order.asc("insertTime"));
            Criterion baseCrit = Restrictions.eq("state", IscSendState.QUEUED);

            if (!currentlyRunning.isEmpty()) {
                // exclude the running send jobs
                Collection<ParmID> runningInits = new ArrayList<ParmID>(
                        currentlyRunning.size());
                for (IscSendRecord record : currentlyRunning) {
                    runningInits.add(record.getParmID());
                }

                baseCrit = Restrictions.and(baseCrit, Restrictions
                        .not(Restrictions.in("parmID", runningInits)));
            }

            queuedCrit.add(baseCrit);

            // possible jobs to run
            List<IscSendRecord> queuedRecords = queuedCrit.list();

            for (IscSendRecord record : queuedRecords) {
                // lock the record
                record = (IscSendRecord) sess.get(IscSendRecord.class,
                        record.getKey(), LockOptions.UPGRADE);

                // double check its still valid
                if (record != null) {
                    sess.delete(record);
                    // can we update primary key in place?? or do we need to
                    // delete then add
                    record = record.clone();
                    record.setState(IscSendState.RUNNING);
                    record.setInsertTime(new Date());
                    sess.save(record);
                    trans.commit();
                    return record;
                }
            }
            trans.commit();
        } catch (Throwable t) {
            handler.error("Querying for available ISC send jobs failed", t);
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (HibernateException e) {
                    handler.handle(
                            Priority.WARN,
                            "Unable to rollback available ISC send job session",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.WARN,
                            "Unable to close available ISC send job session", e);
                }
            }
        }

        return null;
    }

    public static void removeSendJob(IscSendRecord record) {
        Session sess = null;
        Transaction tx = null;

        try {
            CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
            sess = dao.getSessionFactory().openSession();
            tx = sess.beginTransaction();

            // lock the row to ensure no one else will run the
            // upgrade/delete the row
            record = (IscSendRecord) sess.get(IscSendRecord.class,
                    record.getKey(), LockOptions.UPGRADE);

            if (record != null) {
                sess.delete(record);
            }
            tx.commit();
        } catch (Throwable t) {
            handler.error("Unable to remove ISC send job", t);
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(Priority.WARN,
                            "Unable to rollback ISC send job remove session", e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.WARN,
                            "Unable to close ISC send job remove session", e);
                }
            }
        }
    }
}
