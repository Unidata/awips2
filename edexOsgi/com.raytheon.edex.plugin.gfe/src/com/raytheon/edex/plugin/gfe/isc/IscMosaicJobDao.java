package com.raytheon.edex.plugin.gfe.isc;

import java.sql.Date;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.List;
import java.util.Queue;

import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Implements database operations for IscMosaicJobManager.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date        Ticket#     Engineer       Description
 * ----------  ----------  -------------  ----------------------------------
 * 2018-08-08  DCS 19452   dfriedman      Initial creation
 *
 * </pre>
 *
 * @author dfriedman
 */
public class IscMosaicJobDao extends CoreDao {

    private static final long UNPREPARED_TASK_TIMEOUT =
            Long.getLong("iscMosaicJob.unpreparedTaskTimeout", 60)
                    * TimeUtil.MILLIS_PER_SECOND;

    public IscMosaicJobDao() {
        super(DaoConfig.forClass(IscMosaicJobRecord.class));
    }

    public boolean hasExistingJobs() throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(getDaoClass().getName());
        query.addReturnedField("id");
        query.setMaxResults(1);
        return !queryByCriteria(query).isEmpty();
    }

    /**
     * Query either prepared or unprepared jobs, returning them in chronological
     * order.
     *
     * @param prepared
     *            select either prepared or unprepared jobs
     * @return selected jobs in ascending preparation order (if prepared) or
     *         submission order (if unprepared)
     * @throws DataAccessLayerException
     */
    public Queue<Number> queryJobs(boolean prepared)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(getDaoClass().getName());
        query.addQueryParam("prepared", prepared);
        query.addOrder("lastUse", true);
        query.addOrder("inUse", true);
        query.addReturnedField("id");
        List<Number> result = (List<Number>) queryByCriteria(query);
        return new ArrayDeque<>(result);
    }

    /**
     * Lock a job by marking it as in use if it is not already locked or timed
     * out. This must be called within a transaction for the given session.
     *
     * @param unpreparedOnly
     *            if true, only lock the job if it is not already prepared
     * @return the updated job object in the given session or null if the job
     *         could not be locked or @{code unpreparedOnly} was true and the
     *         job was already prepared
     */
    private IscMosaicJobRecord lockJob(int jobID, Session session, long now,
            boolean unpreparedOnly) {
        IscMosaicJobRecord job = (IscMosaicJobRecord) session
                .get(IscMosaicJobRecord.class, jobID, LockOptions.UPGRADE);
        if (job != null && (!unpreparedOnly || !job.isPrepared())
                && (!job.isInUse() || now >= job.getLastUse().getTime()
                        + UNPREPARED_TASK_TIMEOUT)) {
            job.setInUse(true);
            job.setLastUse(new Date(now));
            session.saveOrUpdate(job);
            return job;
        }
        return null;
    }

    /**
     * Attempt to lock a job that may be unprepared.
     *
     * @param jobID
     * @param session
     * @return locked job or null if it could not be locked or it turned out to
     *         be prepared already
     */
    public IscMosaicJobRecord lockUnpreparedJob(int jobID, Session session) {
        Transaction tx = null;
        long now = System.currentTimeMillis();
        try {
            tx = session.beginTransaction();
            IscMosaicJobRecord job = lockJob(jobID, session, now, true);
            tx.commit();
            tx = null;
            return job;
        } finally {
            if (tx != null) {
                tx.rollback();
            }
        }
    }

    /**
     * Update the given job with the current values from the database. This is
     * used to get the remaining set of parms.
     */
    public void refreshJob(IscMosaicJobRecord job, Session session) {
        Transaction tx = session.beginTransaction();
        try {
            session.refresh(job);
            tx.commit();
            tx = null;
        } finally {
            if (tx != null) {
                tx.rollback();
            }
        }
    }

    /**
     * Remove a lock name representing a parm in a job and commit the change to
     * the database.
     */
    public void removeParm(IscMosaicJobRecord job, String lockName,
            Session session) {
        Transaction tx = session.beginTransaction();
        try {
            /*
             * This relies on a cluster lock for lockName in the caller to
             * synchronize the database read.
             */
            session.refresh(job);
            job.getParms().remove(lockName);
            session.save(job);
            tx.commit();
            tx = null;
        } finally {
            if (tx != null) {
                tx.rollback();
            }
        }
    }

    /**
     * Remove the given job from the database
     *
     * @param job
     *            job that has been completed
     * @param session
     * @param lockAndRemoveAll
     *            If true, try to exclusively lock the given job's record and
     *            then remove all job records which have the job's ID as the
     *            leader ID. If false, just remove the given job without
     *            locking.
     * @return true if the job was removed, false if the job was already removed
     *         or could not be locked
     */
    public boolean removeJob(IscMosaicJobRecord job, Session session,
            boolean lockAndRemoveAll) {
        Transaction tx = null;
        try {
            tx = session.beginTransaction();
            if (lockAndRemoveAll) {
                job = lockJob(job.getId(), session, System.currentTimeMillis(),
                        false);
                if (job == null) {
                    // already deleted or about to be deleted by another worker
                    return false;
                }
                session.createQuery(
                        String.format("delete from %s where leader = :id",
                                session.getEntityName(job)))
                        .setInteger("id", job.getId()).executeUpdate();
            } else {
                session.delete(job);
            }
            tx.commit();
            tx = null;
            return true;
        } finally {
            if (tx != null) {
                tx.rollback();
            }
        }
    }

    /** Store one or more job records in a single transaction. */
    public void saveJobs(Collection<IscMosaicJobRecord> jobs, Session session) {
        Transaction tx = null;
        try {
            tx = session.beginTransaction();
            for (IscMosaicJobRecord record : jobs) {
                session.saveOrUpdate(record);
            }
            tx.commit();
            tx = null;
        } finally {
            if (tx != null) {
                tx.rollback();
            }
        }
    }

    /** Retrieve a job record for use within the given Hibernate session. */
    public IscMosaicJobRecord getJob(int jobID, Session session) {
        Transaction tx = session.beginTransaction();
        try {
            IscMosaicJobRecord job = (IscMosaicJobRecord) session
                    .get(IscMosaicJobRecord.class, jobID);
            tx.commit();
            tx = null;
            return job;
        } finally {
            if (tx != null) {
                tx.rollback();
            }
        }
    }

    /**
     * Returns true if all job records grouped by the given leader ID have no
     * parms remaining.  This test is only valid for prepared jobs.
     */
    public boolean isJobComplete(int leaderID) {
        return txTemplate.execute(new TransactionCallback<Boolean>() {
            @Override
            public Boolean doInTransaction(TransactionStatus status) {
                return ((Number) getCurrentSession()
                        .createQuery(
                                "select count(*) from IscMosaicJobRecord job inner join job.parms where leader = :id")
                        .setInteger("id", leaderID).iterate().next())
                                .intValue() == 0;
            }
        });
    }

    /**
     * Returns true if the given job (specified by ID) exists in the database.
     */
    public boolean isJobPresent(int jobID) {
        return txTemplate.execute(new TransactionCallback<Boolean>() {
            @Override
            public Boolean doInTransaction(TransactionStatus status) {
                return getCurrentSession().get(IscMosaicJobRecord.class, jobID) != null;
            }
        });
   }
}
