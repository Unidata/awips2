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
package com.raytheon.uf.edex.backupsvc.database;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data access methods for backup job operations.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2016 5937       tgurney     Initial creation
 * Jul 20, 2017 6352       tgurney     Add versionRequired parameters to
 *                                     createNewJob()
 * Oct  3, 2019 7929       tgurney     Add FIXME for potential issue with
 *                                     large numbers of deferred jobs clogging
 *                                     up the queue
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupJobDao extends CoreDao {

    private static final String DB_NAME = "metadata";

    private static final String POLL_QUERY = "select j1.id id from "
            + " (select max(id) id, jobname from backup_job "
            + " where host = :host group by jobname) j1 "
            + " join (select id, priority from backup_job) j2 "
            + " on j1.id=j2.id order by priority asc";

    private static final String PURGE_OLD_JOBS_QUERY = "delete from backup_job "
            + " where jobname = :jobName and host = :host and id <= :jobId ";

    private static final String PURGE_HOSTS_QUERY = "delete from backup_job "
            + " where host in (:hosts)";

    private static final String PURGE_BLOBS_QUERY = "delete from backup_blob "
            + " where id in "
            + "(select b.id from backup_blob b left join backup_job j"
            + " on b.id = j.backup_blob_id "
            + " where j.backup_blob_id is null)";

    public BackupJobDao() {
        super(DaoConfig.forDatabase(DB_NAME));
    }

    /**
     * Delete this job and any older jobs that it replaced.
     *
     * @param job
     */
    public void removeFinishedJob(BackupJob job) {
        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("jobName", job.getJobName());
        paramMap.put("host", job.getHost());
        paramMap.put("jobId", job.getId());
        executeSQLUpdate(PURGE_OLD_JOBS_QUERY, paramMap);
    }

    /**
     * Delete all jobs except those for the specified hosts. Then delete any
     * orphan blobs.
     *
     * @param hostsToKeep
     *            Keep only the jobs that would run for these hosts.
     */
    public void cleanUp(List<String> hostsToKeep) {
        long t0 = System.currentTimeMillis();
        Object[] result = executeSQLQuery(
                "select distinct host from backup_job");
        if (result != null && result.length > 0) {
            List<String> hostsToRemove = Arrays.stream(result)
                    .map(Object::toString)
                    .filter(item -> !hostsToKeep.contains(item))
                    .collect(Collectors.toList());
            if (!hostsToRemove.isEmpty()) {
                logger.info("Found " + hostsToRemove.size()
                        + " old hosts. Purging old host jobs");
                int purgeCount = executeSQLUpdate(PURGE_HOSTS_QUERY, "hosts",
                        hostsToRemove);
                logger.info("Purged " + purgeCount + " jobs");
            }
        }
        int purgeCount = executeSQLUpdate(PURGE_BLOBS_QUERY);
        long t1 = System.currentTimeMillis();
        logger.info("Purged " + purgeCount + " finished request blobs");
        logger.info("Cleanup took " + TimeUtil.prettyDuration(t1 - t0));
    }

    /**
     * Create a new backup job.
     *
     * @param jobName
     * @param priority
     *            Lower number = higher priority
     * @param blob
     *            The serialized request blob
     * @param hosts
     *            List of hosts to send the request to
     * @param minVersionRequired
     *            Host must have at least this EDEX version to receive this
     *            request
     * @param maxVersionRequired
     *            Host must have no greater than this EDEX version to receive
     *            this request
     */
    public void createNewJob(String jobName, int priority, byte[] blob,
            List<String> hosts, String minVersionRequired,
            String maxVersionRequired) {
        Session session = null;
        Transaction tx = null;
        long now = System.currentTimeMillis();
        try {
            session = getSessionFactory().openSession();
            tx = session.beginTransaction();
            BackupBlob requestBlob = new BackupBlob();
            requestBlob.setBlob(blob);
            session.persist(requestBlob);
            for (String host : hosts) {
                BackupJob job = new BackupJob();
                job.setJobName(jobName);
                job.setPriority(priority);
                job.setRequestBlob(requestBlob);
                job.setCreatedTime(now);
                job.setHost(host);
                job.setMinVersionRequired(minVersionRequired);
                job.setMaxVersionRequired(maxVersionRequired);
                session.persist(job);
            }
            tx.commit();
        } catch (HibernateException e) {
            if (tx != null) {
                tx.rollback();
            }
            throw e;
        } finally {
            if (session != null) {
                session.close();
            }
        }
    }

    /**
     * Get next jobs to run for a single host
     *
     * @param host
     * @return List of jobs
     */
    @SuppressWarnings("unchecked")
    public List<BackupJob> poll(String host) {
        /*
         * FIXME: Returning only the 1000 oldest/highest priority jobs means
         * that in the unlikely event that there are 1000 or more jobs in the
         * queue, and the top 1000 are being held due to version mismatch
         * between sender and receiver, then any jobs behind those will be stuck
         * for a long time. The solution here is not obvious, and this seems
         * pretty unlikely to happen in any operational situation, but if it did
         * happen it could be a big problem.
         */
        Object[] ids = executeSQLQuery(POLL_QUERY, "host", host, 1000);
        if (ids == null || ids.length == 0) {
            return Collections.emptyList();
        }
        List<Long> longIds = Arrays.stream(ids).map(Object::toString)
                .map(Long::valueOf).collect(Collectors.toList());
        Session s = null;
        try {
            s = getSessionFactory().openSession();
            List<BackupJob> jobs = s
                    .createQuery("from BackupJob where id in (:ids)")
                    .setParameterList("ids", longIds).list();
            return jobs;
        } finally {
            if (s != null) {
                s.close();
            }
        }
    }

    /**
     * Get request blob for a specified job
     *
     * @param job
     * @return The serialized request
     */
    public byte[] fetchBlob(BackupJob job) {
        StatelessSession ss = null;
        try {
            ss = getSessionFactory().openStatelessSession();
            BackupBlob blobRecord = (BackupBlob) ss.get(BackupBlob.class,
                    job.getBackupBlobId());
            return blobRecord.getBlob();
        } finally {
            if (ss != null) {
                ss.close();
            }
        }
    }
}
