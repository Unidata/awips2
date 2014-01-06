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
package com.raytheon.uf.edex.database.cluster;

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.database.cluster.handler.IClusterLockHandler;
import com.raytheon.uf.edex.database.cluster.handler.ValidTimeClusterLockHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Cluster locking tools.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2010 #5050      rjpeter     Initial creation from SmartInitTransaction.
 * Aug 26, 2013 #2272      bkowal      Add a function to see if a cluster suffix has
 *                                     been specified via the environment.
 * Dec 13, 2013 2555       rjpeter     Added updateExtraInfoAndLockTime and javadoc.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ClusterLockUtils {
    /*
     * An optional context suffix can be included in an EDEX properties file.
     * This suffix will be appended to the details of each cluster task.
     */
    public static final String CLUSTER_SUFFIX;

    static {
        CLUSTER_SUFFIX = System.getProperty("cluster.suffix") != null ? "-"
                + System.getProperty("cluster.suffix") : "";
    }

    public enum LockState {
        SUCCESSFUL, ALREADY_RUNNING, FAILED, OLD;
    }

    protected static final transient IUFStatusHandler handler = UFStatus
            .getHandler(ClusterLockUtils.class);

    /**
     * 
     * @param taskName
     * @param details
     * @param timeOutOverride
     * @param waitForRunningToFinish
     * @return
     */
    public static ClusterTask lock(String taskName, String details,
            long timeOutOverride, boolean waitForRunningToFinish) {
        return lock(taskName, details, new CurrentTimeClusterLockHandler(
                timeOutOverride, null), waitForRunningToFinish);
    }

    /**
     * Attempts to lock based on the taskName/details and the current system
     * clock for checkTime. If waitForRunningToFinish it will sleep and then
     * attempt to lock again until it achieves a lock other than already
     * running. The waitForRunningToFinish is not part of the main lock logic
     * due to checkTime being keyed off something other than System clock.
     * 
     * @param taskName
     * @param details
     * @param extraInfo
     * @param timeOutOverride
     *            value in milliseconds, if the currentTime > last execution
     *            time + override, it will take the lock even if it is marked as
     *            currently running
     * @param waitForRunningToFinish
     * @return the ClusterTask that was attempted to be locked, see its
     *         LockState for the result of the lock operation. Note: Never
     *         change the ClusterTask that was returned.
     */
    public static ClusterTask lock(String taskName, String details,
            String extraInfo, long timeOutOverride,
            boolean waitForRunningToFinish) {
        return lock(taskName, details, new CurrentTimeClusterLockHandler(
                timeOutOverride, extraInfo), waitForRunningToFinish);
    }

    /**
     * Attempts to lock based on the taskName/details and the specified
     * validTime for checkTime. If waitForRunningToFinish it will sleep and then
     * attempt to lock again until it achieves a lock other than already
     * running. The waitForRunningToFinish is not part of the main lock logic
     * due to checkTime being keyed off something other than System clock. If
     * the validTime is older than the current validTime for the lock, an OLD
     * LockState will be returned.
     * 
     * @param taskName
     * @param details
     * @param validTime
     * @param timeOutOverride
     * @param waitForRunningToFinish
     * @return
     */
    public static ClusterTask lock(String taskName, String details,
            long validTime, long timeOutOverride, boolean waitForRunningToFinish) {
        return lock(taskName, details, new ValidTimeClusterLockHandler(
                validTime, timeOutOverride), waitForRunningToFinish);
    }

    /**
     * Attempts to lock based on the taskName/details and the specified
     * lockHandler. If waitForRunningToFinish it will sleep and then attempt to
     * lock again until it achieves a lock other than already running. The
     * waitForRunningToFinish is not part of the main lock logic due to
     * checkTime being keyed off something other than System clock.
     * 
     * @param taskName
     * @param details
     * @param lockHandler
     * @param waitForRunningToFinish
     * @return
     */
    public static ClusterTask lock(String taskName, String details,
            IClusterLockHandler lockHandler, boolean waitForRunningToFinish) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        ClusterTask ct = null;
        LockState ls = LockState.SUCCESSFUL;
        ClusterTaskPK pk = new ClusterTaskPK();
        pk.setName(taskName);
        pk.setDetails(details);
        boolean tryAgain = true;

        while (tryAgain) {
            tryAgain = false;
            try {
                s = cd.getHibernateTemplate().getSessionFactory().openSession();
                tx = s.beginTransaction();

                ct = getLock(s, pk, true);

                ls = lockHandler.handleLock(ct);
                if (LockState.SUCCESSFUL.equals(ls)) {
                    if (lockHandler.updateLock(ct)) {
                        s.update(ct);
                    }
                }
                tx.commit();
            } catch (Throwable t) {
                handler.handle(Priority.ERROR,
                        "Error processing lock for cluster task [" + taskName
                                + "/" + details + "]", t);

                ls = LockState.FAILED;
                if (ct == null) {
                    ct = new ClusterTask();
                    ct.setId(pk);
                    ct.setRunning(false);
                }

                if (tx != null) {
                    try {
                        tx.rollback();
                    } catch (HibernateException e) {
                        handler.handle(
                                Priority.ERROR,
                                "Error rolling back cluster task lock transaction",
                                e);
                    }
                }
            } finally {
                if (s != null) {
                    try {
                        s.close();
                    } catch (HibernateException e) {
                        handler.handle(Priority.ERROR,
                                "Error closing cluster task lock session", e);
                    }
                }
            }

            if (waitForRunningToFinish && LockState.ALREADY_RUNNING.equals(ls)) {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    // ignore
                }
                tryAgain = true;
            }
        }

        ct.setLockState(ls);
        ct.setLockHandler(lockHandler);
        return ct;
    }

    /**
     * Updates the lock time for the specified lock. IMPORTANT: No tracking is
     * done to ensure caller has lock, so only use when you know you have a
     * valid lock.
     * 
     * @param taskName
     * @param details
     * @param updateTime
     * @return
     */
    public static boolean updateLockTime(String taskName, String details,
            long updateTime) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        ClusterTask ct = null;
        boolean rval = true;

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();
            ClusterTaskPK pk = new ClusterTaskPK();
            pk.setName(taskName);
            pk.setDetails(details);

            ct = getLock(s, pk, true);
            ct.setLastExecution(updateTime);
            s.update(ct);
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing update lock time for cluster task ["
                            + taskName + "/" + details + "]", t);
            rval = false;

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error rolling back cluster task lock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task lock session", e);
                }
            }
        }
        return rval;
    }

    /**
     * Updates the extra info field for a cluster task. IMPORTANT: No tracking
     * is done to ensure caller has lock, so only use when you know you have a
     * valid lock.
     * 
     * @param taskName
     *            The name of the task
     * @param details
     *            The details associated with the task
     * @param extraInfo
     *            The new extra info to set
     * @return True if the update was successful, else false if the update
     *         failed
     */
    public static boolean updateExtraInfo(String taskName, String details,
            String extraInfo) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        ClusterTask ct = null;
        boolean rval = true;

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();
            ClusterTaskPK pk = new ClusterTaskPK();
            pk.setName(taskName);
            pk.setDetails(details);

            ct = getLock(s, pk, true);
            ct.setExtraInfo(extraInfo);
            s.update(ct);
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing update lock time for cluster task ["
                            + taskName + "/" + details + "]", t);
            rval = false;

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error rolling back cluster task lock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task lock session", e);
                }
            }
        }
        return rval;
    }

    /**
     * Updates the extra info and lock time fields for a cluster task.
     * IMPORTANT: No tracking is done to ensure caller has lock, so only use
     * when you know you have a valid lock.
     * 
     * @param taskName
     *            The name of the task
     * @param details
     *            The details associated with the task
     * @param extraInfo
     *            The new extra info to set
     * @oaran lockTime The lock time to set
     * @return True if the update was successful, else false if the update
     *         failed
     */
    public static boolean updateExtraInfoAndLockTime(String taskName,
            String details, String extraInfo, long lockTime) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        ClusterTask ct = null;
        boolean rval = true;

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();
            ClusterTaskPK pk = new ClusterTaskPK();
            pk.setName(taskName);
            pk.setDetails(details);

            ct = getLock(s, pk, true);
            ct.setExtraInfo(extraInfo);
            ct.setLastExecution(lockTime);
            s.update(ct);
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing update lock time for cluster task ["
                            + taskName + "/" + details + "]", t);
            rval = false;

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error rolling back cluster task lock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task lock session", e);
                }
            }
        }
        return rval;
    }

    /**
     * Looks up the specified cluster lock.
     * 
     * @param taskName
     * @param details
     * @return
     */
    public static ClusterTask lookupLock(String taskName, String details) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        ClusterTask ct = null;
        LockState ls = LockState.SUCCESSFUL;
        ClusterTaskPK pk = new ClusterTaskPK();
        pk.setName(taskName);
        pk.setDetails(details);

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();

            ct = getLock(s, pk, true);
            if (ct.isRunning()) {
                ls = LockState.ALREADY_RUNNING;
            }
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing lock lookup for cluster task ["
                            + taskName + "/" + details + "]", t);
            ls = LockState.FAILED;
            if (ct == null) {
                ct = new ClusterTask();
                ct.setId(pk);
                ct.setRunning(false);
            }

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error rolling back cluster task lock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task lock session", e);
                }
            }
        }

        ct.setLockState(ls);

        return ct;
    }

    /**
     * Unlocks the given cluster lock. If clear time is set, time field will be
     * reset to the epoch time. This can be useful when wanting the next check
     * to always succeed.
     * 
     * @param taskName
     * @param details
     * @param clearTime
     * @return
     */
    public static boolean unlock(ClusterTask ct, boolean clearTime) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        boolean rval = true;

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();

            ClusterTask dbCt = getLock(s, ct.getId(), true);
            if (ct.getLockHandler() != null) {
                ct.getLockHandler().unlock(dbCt, clearTime);
            } else {
                dbCt.setRunning(false);
            }
            s.update(dbCt);
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing unlock for cluster task ["
                            + ct.getId().getName() + "/"
                            + ct.getId().getDetails() + "]", t);
            rval = false;

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(
                            Priority.ERROR,
                            "Error rolling back cluster task unlock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task unlock session", e);
                }
            }
        }

        return rval;
    }

    /**
     * Standard unlock. Doesn't handle clearing the time due to different
     * implementations of IClusterLockHandler.
     * 
     * @param taskName
     * @param details
     * @param clearTime
     * @return
     */
    public static boolean unlock(String taskName, String details) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        boolean rval = true;

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();

            ClusterTaskPK pk = new ClusterTaskPK();
            pk.setName(taskName);
            pk.setDetails(details);
            ClusterTask ct = getLock(s, pk, true);
            ct.setRunning(false);
            s.update(ct);
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing unlock for cluster task [" + taskName
                            + "/" + details + "]", t);
            rval = false;

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(
                            Priority.ERROR,
                            "Error rolling back cluster task unlock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task unlock session", e);
                }
            }
        }

        return rval;
    }

    /**
     * Deletes the specified cluster lock.
     * 
     * @param taskName
     * @param details
     * @return
     */
    public static boolean deleteLock(String taskName, String details) {
        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        boolean rval = true;
        ClusterTaskPK pk = new ClusterTaskPK();
        pk.setName(taskName);
        pk.setDetails(details);

        try {
            s = cd.getHibernateTemplate().getSessionFactory().openSession();
            tx = s.beginTransaction();

            ClusterTask ct = getLock(s, pk, false);
            if (ct != null) {
                s.delete(ct);
            }
            tx.commit();
        } catch (Throwable t) {
            handler.handle(Priority.ERROR,
                    "Error processing delete lock for cluster task ["
                            + taskName + "/" + details + "]", t);
            rval = false;

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (HibernateException e) {
                    handler.handle(
                            Priority.ERROR,
                            "Error rolling back cluster task delete lock transaction",
                            e);
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task delete lock  session",
                            e);
                }
            }
        }

        return rval;
    }

    /**
     * Looks up and returns the specified cluster lock. If the lock does not
     * exist and create flag is set, the lock will be created. This is done
     * using a Master lock to ensure isolation among all transactions.
     * 
     * @param s
     * @param pk
     * @param create
     * @return
     * @throws HibernateException
     */
    private static ClusterTask getLock(Session s, ClusterTaskPK pk,
            boolean create) throws HibernateException {
        ClusterTask ct = (ClusterTask) s.get(ClusterTask.class, pk,
                LockOptions.UPGRADE);
        if ((ct == null) && create) {
            getMasterLock(s);

            // now have master lock, verify new row hasn't already been
            // created
            ct = (ClusterTask) s
                    .get(ClusterTask.class, pk, LockOptions.UPGRADE);

            if (ct == null) {
                ct = new ClusterTask();
                ct.setId(pk);
                ct.setRunning(false);
                s.save(ct);
            }
        }

        return ct;
    }

    /**
     * Returns the master lock.
     * 
     * @param s
     * @return
     * @throws HibernateException
     */
    private static ClusterTask getMasterLock(Session s)
            throws HibernateException {
        ClusterTaskPK masterNewRowLockId = new ClusterTaskPK();
        masterNewRowLockId.setName("MasterLock");
        masterNewRowLockId.setDetails("NewRowLock");
        ClusterTask masterLock = (ClusterTask) s.get(ClusterTask.class,
                masterNewRowLockId, LockOptions.UPGRADE);

        if (masterLock == null) {
            handler.handle(Priority.WARN,
                    "MasterLock for new row missing from cluster task.  Attempting to create.");
            masterLock = new ClusterTask();
            masterLock.setId(masterNewRowLockId);
            masterLock.setRunning(false);
            s.save(masterLock);
        }

        return masterLock;
    }

    /**
     * Returns all cluster locks that match the specified name.
     * 
     * @param name
     * @return
     */
    @SuppressWarnings("unchecked")
    public static List<ClusterTask> getLocks(String name) {
        StatelessSession sess = null;
        List<ClusterTask> tasks = null;

        try {
            CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
            sess = dao.getSessionFactory().openStatelessSession();

            Criteria crit = sess.createCriteria(ClusterTask.class);
            Criterion nameCrit = Restrictions.eq("id.name", name);
            crit.add(nameCrit);
            tasks = crit.list();
        } catch (Throwable e) {
            handler.handle(Priority.ERROR,
                    "Error retrieving cluster locks for name: " + name, e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (HibernateException e) {
                    handler.handle(Priority.ERROR,
                            "Error closing cluster task getLocks session", e);
                }
            }
        }

        return tasks;
    }
}
