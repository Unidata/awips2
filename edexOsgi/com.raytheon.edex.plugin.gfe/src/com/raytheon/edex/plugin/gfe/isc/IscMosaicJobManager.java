/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.edex.plugin.gfe.isc;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Session;
import org.hibernate.UnresolvableObjectException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.IContextStateProcessor;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

import jep.JepConfig;
import jep.JepException;

/**
 * Manages the ordering of ISC mosaic jobs and individual parms to avoid
 * unnecessary waiting on cluster task locks.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 08, 2018 19452      dfriedman    Initial creation
 * Sep 09, 2022 23257      dgilling     Rewrite to no longer reuse PythonScript
 *                                      instances between site ids.
 * Aug 07, 2024 2037700    tgurney      Fix hang at shutdown when less than max
 *                                      threads were started
 * Oct 21, 2025 2039719    njensen      Catch UnresolvableObjectException when
 *                                      refreshing job in processOneParm()
 * Feb 17, 2026 2040999    njensen      Fix 2039719 in a smarter way to avoid
 *                                      infinite loop of refreshing job and
 *                                      sending notifications
 *
 * </pre>
 *
 * @author dfriedman
 */

public class IscMosaicJobManager implements IContextStateProcessor {

    private static final Logger logger = LoggerFactory
            .getLogger(IscMosaicJobManager.class);

    private static final String CLUSTER_LOCK_NAME = "ISC Write Lock";

    private static final long CLUSTER_TASK_TIMEOUT = Long
            .getLong("iscMosaicJob.clusterTaskTimeout", 400)
            * TimeUtil.MILLIS_PER_SECOND;

    private static final String NOTIFY_ROUTE_NAME = "iscMosaicStatusNotifyRoute";

    private static final String PREPARE_METHOD_NAME = "prepareMosaicRequest";

    private static final String PROCESS_PARM_METHOD_NAME = "processParm";

    private static final String CLEAN_UP_JOB_METHOD_NAME = "cleanUpJob";

    private static final String DELETE_INPUT_ARG_NAME = "deleteInput";

    private static final String INPUT_FILES_ARG_NAME = "inFiles";

    private static final Comparator<ClusterTask> CT_LAST_EXEC_COMPARATOR = Comparator
            .comparingLong(ClusterTask::getLastExecution);

    private final ThreadPoolExecutor threadPool;

    private final ConcurrentMap<Integer, MosaicJob> waitingJobs;

    /**
     * Used to wake up work threads that are waiting for a WAKEUP notification.
     */
    private final Object wakeSignal;

    /**
     * Used by worker threads to determine if a WAKEUP has been received while
     * running.
     */
    private final AtomicInteger wakeupCounter;

    /**
     * Controls whether worker threads will be started and continue running.
     */
    private final AtomicBoolean running;

    private final IscMosaicJobDao dao;

    /**
     * Used to communicate mosaic processing events between EDEX nodes.
     * <p>
     * There are two types of messages:
     * <ul>
     * <li>WAKEUP - Indicates worker threads should resume processing because a
     * cluster lock has been released or a new job has been prepared.
     * <li>COMPLETED_JOB - Indicates a thread waiting for a job to complete
     * should resume. Also includes an error message.
     * </ul>
     */
    @DynamicSerialize
    private static class MosaicStatusMessage {

        private enum Type {
            WAKEUP, COMPLETED_JOB
        }

        @DynamicSerializeElement
        private Type type;

        @DynamicSerializeElement
        private int jobID;

        @DynamicSerializeElement
        private String message;

        public MosaicStatusMessage() {
        }

        private MosaicStatusMessage(Type type) {
            this(type, 0, null);
        }

        private MosaicStatusMessage(Type type, int jobID, String message) {
            this.type = type;
            this.jobID = jobID;
            this.message = message;
        }

        public Type getType() {
            return type;
        }

        public void setType(Type type) {
            this.type = type;
        }

        public int getJobID() {
            return jobID;
        }

        public void setJobID(int jobID) {
            this.jobID = jobID;
        }

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }
    }

    /**
     * Provides an interface to set up and submit jobs.
     */
    public static class MosaicJob {

        private IscMosaicJobRecord record;

        /**
         * Contains jobs with actual parm sets.
         */
        private List<MosaicJob> jobs;

        private AtomicBoolean done;

        private String result;

        /** cluster lock names for remaining parms to be processed */
        private Set<String> lockNames = new HashSet<>();

        private MosaicJob() {
            this(new IscMosaicJobRecord());
            record.setLastUse(TimeUtil.newDate());
        }

        public MosaicJob(IscMosaicJobRecord record) {
            this.record = record;
            this.done = new AtomicBoolean();
        }

        public void setSiteID(String siteID) {
            record.setSite(siteID);
        }

        public void setArgs(Map<String, Object> args) throws IOException {
            record.setArgsMap(args);
        }

        public Map<String, Object> getArgs() throws IOException {
            return record.getArgsMap();
        }

        /**
         * Create a new job associated with this leader job.
         *
         * @param data
         *            data to make available to the python script that processes
         *            the job (i.e., the ISC mosaic args dictionary)
         */
        public MosaicJob createAssociatedJob(Object data) throws IOException {
            if (data == null) {
                throw new IllegalArgumentException("args must not be null");
            }
            if (jobs == null) {
                jobs = new ArrayList<>();
            }
            MosaicJob newJob = new MosaicJob();
            IscMosaicJobRecord newRecord = newJob.record;
            newRecord.setPrepared(true);
            newRecord.setSite(record.getSite());
            newRecord.setLeader(record.getId());
            newRecord.setArgsMap((Map<String, Object>) data);
            newRecord.setLastUse(record.getLastUse());
            jobs.add(newJob);
            return newJob;
        }

        /**
         * Return the job records that need to be stored in the database after
         * running the preparation step. Does not include the @{code record} of
         * this @{code MosaicJob} instance.
         */
        List<IscMosaicJobRecord> getJobsToStore()
                throws SerializationException {
            List<IscMosaicJobRecord> records = new ArrayList<>();
            if (jobs != null) {
                for (MosaicJob job : jobs) {
                    if (job.lockNames.isEmpty()) {
                        logger.warn(String.format(
                                "job with args %s has no parms to process",
                                record.getArgs()));
                        continue;
                    }
                    job.record.setParms(job.lockNames);
                    records.add(job.record);
                }
            }
            return records;
        }

        /**
         * Add the given lock name to the set of lock names.
         *
         * @param lockName
         */
        public synchronized void addLockName(String lockName) {
            lockNames.add(lockName);
        }
    }

    public IscMosaicJobManager(ThreadPoolExecutor threadPool) {
        this.threadPool = threadPool;
        this.waitingJobs = new ConcurrentHashMap<>();
        this.wakeSignal = new Object();
        this.wakeupCounter = new AtomicInteger();
        this.running = new AtomicBoolean();
        this.dao = new IscMosaicJobDao();
    }

    /**
     * Check if there are any existing jobs and, if so, start worker threads.
     * This is intended to be called at startup.
     */
    public void checkExistingJobs() {
        try {
            if (dao.hasExistingJobs()) {
                wakeUpExecutors();
            }
        } catch (Exception e) {
            logger.error("Failed to check for existing jobs: " + e.toString(),
                    e);
        }
    }

    /**
     * Create a new job
     *
     */
    public MosaicJob createJob() {
        return new MosaicJob();
    }

    /**
     * Submit a new job to the manager. Writes a job record to the database,
     * marked as "unprepared". Sends a notification that new work is available.
     * After calling this, clients can call {@link waitForJob} to wait for the
     * job to complete.
     *
     * @param job
     */
    public void submit(MosaicJob job) {
        IscMosaicJobRecord record = job.record;
        if (StringUtils.isEmpty(record.getSite())
                || StringUtils.isEmpty(record.getArgs())) {
            throw new IllegalArgumentException(
                    "job site and args must not be null");
        }

        record.setPrepared(false);
        record.setInUse(false);
        record.setLastUse(TimeUtil.newDate());

        synchronized (waitingJobs) {
            dao.create(record);
            waitingJobs.put(record.getId(), job);
        }
        notifyExecutors(
                new MosaicStatusMessage(MosaicStatusMessage.Type.WAKEUP));
    }

    public String waitForJob(MosaicJob job) throws InterruptedException {
        synchronized (job) {
            while (!job.done.get()) {
                job.wait(CLUSTER_TASK_TIMEOUT / 2);
                if (!job.done.get() && !dao.isJobPresent(job.record.getId())) {
                    job.done.set(true);
                    waitingJobs.remove(job.record.getId());
                    logger.error("Did not receive a notification for completed "
                            + getJobDescription(job.record));
                }
            }
        }
        return job.result;
    }

    /**
     * Return the collection of files to be removed when the job is completed.
     * Always returns a non-null collection.
     *
     * @param job
     * @return
     */
    private Collection<String> getFilesToDelete(IscMosaicJobRecord job) {
        Map<String, Object> args = null;
        try {
            args = job.getArgsMap();
        } catch (IOException e) {
            logger.error("failed to get files to delete for %s: %s",
                    getJobDescription(job), e);
        }
        if (args != null) {
            Object deleteInput = args.get(DELETE_INPUT_ARG_NAME);
            if (deleteInput instanceof Boolean && (Boolean) deleteInput) {
                Object inputSpec = args.get(INPUT_FILES_ARG_NAME);
                if (inputSpec instanceof Collection) {
                    Collection<?> inputFilesCollection = (Collection<?>) inputSpec;
                    Collection<String> result = new ArrayList<>(
                            inputFilesCollection.size());
                    for (Object o : inputFilesCollection) {
                        if (o instanceof String) {
                            result.add((String) o);
                        }
                    }
                    return result;
                }
            }
        }
        return Collections.emptySet();
    }

    private String getJobDescription(IscMosaicJobRecord job) {
        return String.format("job %d with args %s", job.getId(), job.getArgs());
    }

    /**
     * Remove the given job from the database, clean up input files (if cleanup
     * is specified in job args), and signal notification of job completion.
     *
     * @param job
     *            job that has been completed
     * @param message
     *            result message passed to requesting client
     * @param session
     * @param lockAndRemoveAll
     *            If true, try to exclusively lock the given job's record and
     *            then remove all job records which have the job's ID as the
     *            leader ID. If false, just remove the given job without
     *            locking.
     */
    private void complete(IscMosaicJobRecord job, String message,
            Session session, boolean lockAndRemoveAll) {
        session.evict(job);
        try {
            if (!dao.removeJob(job, session, lockAndRemoveAll)) {
                return;
            }
        } catch (Exception e) {
            throw new RuntimeException(
                    String.format("error while completing %s: %s",
                            getJobDescription(job), e),
                    e);
        }
        for (String path : getFilesToDelete(job)) {
            try {
                Files.delete(Paths.get(path));
            } catch (Exception e) {
                logger.error(String.format("error deleting %s: %s", path, e),
                        e);
            }
        }
        notifyExecutors(new MosaicStatusMessage(
                MosaicStatusMessage.Type.COMPLETED_JOB, job.getId(), message));
    }

    /**
     * Prepares new jobs. For each unprepared job, run a method in iscMosaic.py
     * to create a prepared job for each input file and argument set
     * combination. Each prepared job will have a set of lockNames that will be
     * needed to process each of its parms.
     * <p>
     * If the preparation set fails, the job record is removed from the database
     * and a notification is sent.
     *
     * @return
     * @throws DataAccessLayerException
     */
    private boolean prepareJobs() throws DataAccessLayerException {
        Queue<Number> jobIDs = dao.queryJobs(false);
        Session session = dao.getSession();
        PythonScript script = null;

        try {
            boolean didWork = false;
            String prevSiteId = StringUtils.EMPTY;

            while (running.get() && !jobIDs.isEmpty()) {
                int jobID = jobIDs.poll().intValue();
                IscMosaicJobRecord job = dao.lockUnpreparedJob(jobID, session);
                if (job == null) {
                    continue;
                }
                didWork = true;
                List<IscMosaicJobRecord> jobsToStore;
                try {
                    Map<String, Object> args = job.getArgsMap();
                    MosaicJob wrapper = new MosaicJob(job);
                    args.put("job", wrapper);

                    String siteId = job.getSite();
                    if (!siteId.equals(prevSiteId)) {
                        if (script != null) {
                            try {
                                script.dispose();
                            } catch (JepException e) {
                                logger.warn(
                                        "Error disposing of PythonScript instance.",
                                        e);
                            }
                        }

                        script = getPythonScript(siteId);
                        prevSiteId = siteId;
                    }
                    script.execute(PREPARE_METHOD_NAME, args);
                    jobsToStore = wrapper.getJobsToStore();
                } catch (Exception e) {
                    String message = String.format(
                            "iscMosaic script failed during preparation of %s: %s",
                            getJobDescription(job), e);
                    logger.error(message, e);
                    complete(job, message, session, false);
                    continue;
                }
                if (jobsToStore.isEmpty()) {
                    complete(job, null, session, false);
                    continue;
                }
                job.setPrepared(true);
                job.setLeader(job.getId());
                job.setInUse(false);
                jobsToStore.add(job);
                try {
                    dao.saveJobs(jobsToStore, session);
                } catch (Exception e) {
                    throw new RuntimeException(String.format("error saving %s",
                            getJobDescription(job)), e);
                }
                notifyExecutors(new MosaicStatusMessage(
                        MosaicStatusMessage.Type.WAKEUP));
            }
            return didWork;
        } finally {
            session.close();

            if (script != null) {
                try {
                    script.dispose();
                } catch (JepException e) {
                    logger.warn("Error disposing of PythonScript instance.", e);
                }
            }
        }
    }

    /**
     * Process parms of prepared jobs.
     *
     * @return
     * @throws DataAccessLayerException
     */
    private boolean processParms() throws DataAccessLayerException {
        boolean didWork = false;
        PythonScript script = null;
        String prevSiteId = StringUtils.EMPTY;

        // Get prepared jobs and their parm sets
        Collection<Number> jobs = dao.queryJobs(true);
        Session session = dao.getSession();
        try {
            for (Number jobID : jobs) {
                if (!running.get()) {
                    break;
                }
                IscMosaicJobRecord job = null;
                try {
                    job = dao.getJob(jobID.intValue(), session);
                    if (job == null) {
                        // already deleted
                        continue;
                    }
                } catch (Exception e) {
                    throw new RuntimeException(
                            String.format("error loading job %d", jobID), e);
                }

                /*
                 * Get the list of current locks to find candidate parms to
                 * process.
                 */
                List<ClusterTask> locks = ClusterLockUtils
                        .getLocks(CLUSTER_LOCK_NAME);
                /*
                 * Sort locks in order of last execution time so that if a stale
                 * lock is overridden in @{code lockNextParm}, it will be the
                 * oldest possible lock.
                 */
                locks.sort(CT_LAST_EXEC_COMPARATOR);

                String siteId = job.getSite();
                if (!siteId.equals(prevSiteId)) {
                    if (script != null) {
                        try {
                            script.close();
                        } catch (JepException e) {
                            logger.warn(
                                    "Error disposing of PythonScript instance.",
                                    e);
                        }
                    }

                    try {
                        script = getPythonScript(siteId);
                        prevSiteId = siteId;
                    } catch (JepException e) {
                        String message = String.format(
                                "iscMosaic script failed during preparation of %s",
                                getJobDescription(job));
                        logger.error(message, e);
                        // move on to next jobID from jobs
                        continue;
                    }
                }

                try {
                    MosaicJob jobWrapper = new MosaicJob(job);

                    while (running.get() && !job.getParms().isEmpty()) {
                        boolean locked = processOneParm(jobWrapper, locks,
                                session, script);
                        didWork |= locked;
                        if (!locked) {
                            /*
                             * If a cluster lock could not be obtained, do not
                             * keep looping. This will try another job, but the
                             * number of jobs is limited.
                             */
                            break;
                        }
                    }
                } finally {
                    // Call the script's cleanup method to close the NetCDF file
                    // if needed.
                    try {
                        script.execute(CLEAN_UP_JOB_METHOD_NAME,
                                Collections.emptyMap());
                    } catch (Exception e) {
                        String error = String.format(
                                "iscMosaic script failed during cleanup for %s: %s",
                                getJobDescription(job), e);
                        logger.error(error, e);
                    }
                }

                /*
                 * If there are no more parms associated with any job record
                 * with the same leader ID as this job, complete the overall
                 * job.
                 */
                if (dao.isJobComplete(job.getLeader())) {
                    IscMosaicJobRecord leader = session
                            .get(IscMosaicJobRecord.class, job.getLeader());
                    if (leader != null) {
                        complete(leader, null, session, true);
                    }
                }
            }
        } finally {
            session.close();

            if (script != null) {
                try {
                    script.close();
                } catch (JepException e) {
                    logger.warn("Error disposing of PythonScript instance.", e);
                }
            }
        }

        return didWork;
    }

    /**
     * Process at most one parm for the given job.
     * <p>
     * First, try to obtain a free cluster lock for one of the (apparent)
     * remaining parms in the job. If a lock was obtained, refresh the job's
     * list of parms to make sure another worker has not processed it. If the
     * parm is still available, process it and then remove it from the job's
     * list of parms in memory and in the database. The cluster lock
     * synchronizes this read and delete cycle for the given lock name.
     *
     * @param jobWrapper
     * @param locks
     * @param session
     * @param script
     * @return true if a lock was obtained (Does not imply a parm was actually
     *         processed.)
     */
    private boolean processOneParm(MosaicJob jobWrapper,
            List<ClusterTask> locks, Session session, PythonScript script) {
        IscMosaicJobRecord job = jobWrapper.record;
        ClusterTask clusterTask = lockNextParm(job.getParms(), locks);
        try {
            if (clusterTask != null
                    && clusterTask.getLockState() == LockState.SUCCESSFUL) {
                /*
                 * Check if the lock name is still in the job's set.
                 *
                 * The lock name cannot be removed at this point because that
                 * could allow another worker to consider the job to be
                 * completed while the parm is still being processed. The lock
                 * name is removed below, after processing.
                 */
                dao.refreshJob(job, session);
                String lockName = clusterTask.getId().getDetails();
                boolean haveParm = job.getParms().contains(lockName);
                if (!haveParm) {
                    return true;
                }
                try {
                    Map<String, Object> args = Map.of("job", jobWrapper,
                            "lockName", lockName);
                    script.execute(PROCESS_PARM_METHOD_NAME, args);
                } catch (Exception e) {
                    String error = String.format(
                            "iscMosaic script failed for lock name %s in %s: %s",
                            lockName, getJobDescription(job), e);
                    logger.error(error, e);
                }
                try {
                    dao.removeParm(job, lockName, session);
                } catch (Exception e) {
                    throw new RuntimeException(
                            String.format("error removing parm %s of %s",
                                    lockName, getJobDescription(job)),
                            e);
                }
                return true;
            } else {
                if (clusterTask != null
                        && clusterTask.getLockState() == LockState.FAILED) {
                    logger.error(String.format(
                            "Attempt to take cluster lock %s failed",
                            clusterTask.getId().getDetails()));
                }
                return false;
            }
        } finally {
            if (clusterTask != null
                    && clusterTask.getLockState() == LockState.SUCCESSFUL) {
                ClusterLockUtils.unlock(clusterTask, true);
                /*
                 * Wake up other executors that may now be able to process a
                 * parm with this name.
                 */
                notifyExecutors(new MosaicStatusMessage(
                        MosaicStatusMessage.Type.WAKEUP));
            }
        }
    }

    private Runnable createWorkerJob() {
        return () -> {

            int lastCounter = wakeupCounter.get();

            while (running.get()) {
                boolean didWork = false;

                try {
                    didWork = prepareJobs();
                    didWork = processParms() || didWork;
                } catch (UnresolvableObjectException e) {
                    /*
                     * There's a race condition where another worker, probably
                     * on another machine in the cluster, completed the parm and
                     * it was the last parm in the set, so the job was removed.
                     * Since job.getParms() is just in memory it may not be up
                     * to date with the latest removed, which is why refreshJob()
                     * is called in processOneParm(). But you can get an
                     * UnresolvableObjectException on dao.refreshJob(job,
                     * session) if the other worker removed the job entirely as
                     * it was complete and this worker is trying to check if the
                     * parm still needs processed. We catch the exception up
                     * here to avoid an infinite loop that would occur if we
                     * caught it inside processOneParm(), where it tries to
                     * refresh the job, catches the exception, tries to refresh
                     * the job, catches the exception, and so on.
                     */
                    logger.debug("Job not found in database, "
                            + "most likely completed and deleted by another thread that is "
                            + "on another machine in the cluster", e);
                } catch (Exception e) {
                    logger.error("Error occurred running iscMosaic.", e);
                }

                /*
                 * If there is work left (previously known or new jobs), but we
                 * did not do anything due to being unable to take a cluster
                 * lock, wait for a wakeup signal. The wakeup can be for the
                 * notification of an unlock operation or creation of additional
                 * work.
                 *
                 * In case something goes wrong that prevents notification
                 * (Unexpected error, message queue or database failure, etc.),
                 * only wait a limited amount of time.
                 */
                if (!didWork) {
                    synchronized (wakeSignal) {
                        if (running.get()
                                && lastCounter == wakeupCounter.get()) {
                            try {
                                wakeSignal.wait(CLUSTER_TASK_TIMEOUT / 2);
                            } catch (InterruptedException e) {
                                // just continue
                            }
                        }
                        lastCounter = wakeupCounter.get();
                    }
                }
            }
        };
    }

    /**
     * Attempt to lock the next parm for a job without waiting. If none of the
     * desired locks are free, attempt to override a stale lock. If no stale
     * locks can be overridden, return null.
     *
     * @param lockNames
     *            remaining lock names for parms of a job that need to be
     *            processed
     * @param locks
     *            recent list of cluster locks ordered by getLastExecution()
     * @return ClusterTask in case of success or a hard failure, null if no
     *         locks are currently free.
     */
    private static ClusterTask lockNextParm(Set<String> lockNames,
            List<ClusterTask> locks) {
        Set<String> reservedLocks = new HashSet<>();
        List<ClusterTask> fallbackCTs = new ArrayList<>();
        long now = System.currentTimeMillis();
        for (ClusterTask ct : locks) {
            String lockName = ct.getId().getDetails();
            if (ct.isRunning()) {
                reservedLocks.add(lockName);
                if (lockNames.contains(lockName) && ct.getLastExecution()
                        + CLUSTER_TASK_TIMEOUT <= now) {
                    fallbackCTs.add(ct);
                }
            }
        }
        ClusterTask ctToUse = null;
        for (String lockName : lockNames) {
            if (!reservedLocks.contains(lockName)) {
                ClusterTask ct = ClusterLockUtils.lock(CLUSTER_LOCK_NAME,
                        lockName, CLUSTER_TASK_TIMEOUT, false);
                /*
                 * Return both success and hard failures to the caller. In the
                 * case of ALREADY_RUNNING, try to obtain another lock.
                 */
                LockState lockState = ct.getLockState();
                if (lockState != LockState.ALREADY_RUNNING) {
                    ctToUse = ct;
                    break;
                }
            }
        }
        if (ctToUse == null && !fallbackCTs.isEmpty()) {
            for (ClusterTask fallbackCT : fallbackCTs) {
                ClusterTask ct = ClusterLockUtils.lock(CLUSTER_LOCK_NAME,
                        fallbackCT.getId().getDetails(), CLUSTER_TASK_TIMEOUT,
                        false);
                LockState lockState = ct.getLockState();
                if (lockState != LockState.ALREADY_RUNNING) {
                    ctToUse = ct;
                    break;
                }
            }
        }
        return ctToUse;
    }

    /**
     * Send the given message to all request processes. When the message is
     * received, it is handled in {@link handleStatusMessage()}.
     *
     * @param msg
     */
    private static void notifyExecutors(MosaicStatusMessage msg) {
        try {
            EDEXUtil.getMessageProducer().sendAsync(NOTIFY_ROUTE_NAME,
                    SerializationUtil.transformToThrift(msg));
        } catch (EdexException | SerializationException e) {
            logger.error(String.format("failed to send to %s: %s",
                    NOTIFY_ROUTE_NAME, e), e);
        }
    }

    /**
     * Called in response to an inter-process notification.
     *
     * @param msg
     */
    public void handleStatusMessage(MosaicStatusMessage msg) {
        if (msg != null
                && msg.getType() == MosaicStatusMessage.Type.COMPLETED_JOB) {
            MosaicJob job;
            synchronized (waitingJobs) {
                job = waitingJobs.remove(msg.getJobID());
            }

            if (job != null) {
                synchronized (job) {
                    job.done.set(true);
                    job.result = msg.getMessage();
                    job.notifyAll();
                }
            }
        } else {
            wakeUpExecutors();
        }
    }

    /**
     * Wake up executors in this process, starting them if they are not running.
     */
    private void wakeUpExecutors() {
        synchronized (wakeSignal) {
            if (running.get()) {
                int nToStart = threadPool.getMaximumPoolSize()
                        - threadPool.getActiveCount();
                for (int i = 0; i < nToStart; ++i) {
                    threadPool.submit(createWorkerJob());
                }
                wakeupCounter.incrementAndGet();
                wakeSignal.notifyAll();
            }
        }
    }

    private PythonScript getPythonScript(String siteId) throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File scriptLoc = pathMgr.getFile(ctx,
                LocalizationUtil.join("gfe", "python", "isc", "iscMosaic.py"));

        String includePath = PyUtil.buildJepIncludePath(
                PythonIncludePathUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getVtecIncludePath(siteId),
                GfePyIncludeUtil.getIscScriptsIncludePath(),
                GfePyIncludeUtil.getGfeConfigIncludePath(siteId));

        return new PythonScript(new JepConfig().setIncludePath(includePath)
                .setClassLoader(getClass().getClassLoader()),
                scriptLoc.getPath());
    }

    @Override
    public void preStart() {
        running.set(true);
    }

    @Override
    public void postStart() {
        checkExistingJobs();
    }

    /**
     * Indicate workers should not do any more work. Wake any workers that may
     * be running so that they can terminate.
     */
    @Override
    public void preStop() {
        synchronized (wakeSignal) {
            running.set(false);
            wakeSignal.notifyAll();
        }
    }

    /**
     * Wait for worker threads to terminate. Will wait no more than 30 seconds
     * or CLUSTER_TASK_TIMEOUT if that is less than 30 seconds. Can't wait too
     * long or EDEX will get killed.
     */
    @Override
    public void postStop() {
        try {
            threadPool.shutdownNow();
            boolean didShutdown = threadPool.awaitTermination(
                    Math.min(CLUSTER_TASK_TIMEOUT, 30000),
                    TimeUnit.MILLISECONDS);
            if (!didShutdown) {
                logger.warn("Timed out waiting for ISC mosaic jobs to finish");
            }
        } catch (InterruptedException e) {
            logger.warn("IscMosaicJobManager postStop() interrupted.", e);
        }
    }
}
