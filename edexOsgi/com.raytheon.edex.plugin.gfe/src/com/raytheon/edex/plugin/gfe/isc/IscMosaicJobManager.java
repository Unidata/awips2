/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.edex.plugin.gfe.isc;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.hibernate.Session;

import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.IContextStateProcessor;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

import jep.JepException;

/**
 * Manages the ordering of ISC mosaic jobs and individual parms to avoid
 * unnecessary waiting on cluster task locks.
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
public class IscMosaicJobManager implements IContextStateProcessor {

    private static final String CLUSTER_LOCK_NAME = "ISC Write Lock";

    private static final long CLUSTER_TASK_TIMEOUT =
            Long.getLong("iscMosaicJob.clusterTaskTimeout", 400)
                    * TimeUtil.MILLIS_PER_SECOND;

    private static final String NOTIFY_ROUTE_NAME = "iscMosaicStatusNotifyRoute";
    private static final String PREPARE_METHOD_NAME = "prepareMosaicRequest";
    private static final String PROCESS_PARM_METHOD_NAME = "processParm";
    private static final String CLEAN_UP_JOB_METHOD_NAME = "cleanUpJob";
    private static final String DELETE_INPUT_ARG_NAME = "deleteInput";
    private static final String INPUT_FILES_ARG_NAME = "inFiles";

    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(IscMosaicJobManager.class);

    private static final Comparator<ClusterTask> CT_LAST_EXEC_COMPARATOR =
            Comparator.comparingLong(ClusterTask::getLastExecution);

    private PythonJobCoordinator<IscScript> threadPool;

    /**
     * Number or workers to start in {@code threadPool}. PythonJobCoordinator
     * does not expose the maximum number of threads, so this must be set
     * separately
     **/
    private int nThreads;

    private AtomicInteger nActiveThreads = new AtomicInteger();

    private ConcurrentHashMap<Integer, MosaicJob> waitingJobs = new ConcurrentHashMap<>();

    /**
     * Used to wake up work threads that are waiting for a WAKEUP notification.
     */
    private Object wakeSignal = new Object();

    /**
     * Used by worker threads to determine if a WAKEUP has been received while
     * running.
     */
    private volatile int wakeupCounter;

    /**
     * Controls whether worker threads will be started and continue running.
     */
    private volatile boolean running;

    private IscMosaicJobDao dao;

    /**
     * Used to communicate mosaic processing events between EDEX nodes.
     * <p>
     * There are two types of messages:
     * <p>
     * WAKEUP - Indicates worker threads should resume processing because a
     * cluster lock has been released or a new job has been prepared.
     * <p>
     * COMPLETED_JOB - Indicates a thread waiting for a job to complete should
     * resume. Also includes an error message.
     */
    @DynamicSerialize
    private static class MosaicStatusMessage {

        private static enum Type { WAKEUP, COMPLETED_JOB };

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
    public class MosaicJob {
        private IscMosaicJobRecord record;

        /**
         * Contains jobs with actual parm sets.
         */
        private List<MosaicJob> jobs;

        private volatile boolean done;

        private String result;

        /** cluster lock names for remaining parms to be processed */
        private Set<String> lockNames = new HashSet<>();

        private MosaicJob() {
            this(new IscMosaicJobRecord());
            record.setLastUse(new Date(System.currentTimeMillis()));
        }

        public MosaicJob(IscMosaicJobRecord record) {
            this.record = record;
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
                throw new NullPointerException("args must not be null");
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
        List<IscMosaicJobRecord> getJobsToStore() throws SerializationException {
            List<IscMosaicJobRecord> records = new ArrayList<>();
            if (jobs != null) {
                for (MosaicJob job : jobs) {
                    if (job.lockNames.isEmpty()) {
                        statusHandler.warn(String.format(
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

    public IscMosaicJobManager(PythonJobCoordinator<IscScript> threadPool, int nThreads) {
        this.threadPool = threadPool;
        this.nThreads = nThreads;
        dao = new IscMosaicJobDao();
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
            statusHandler.error(
                    "failed to check for existing jobs: " + e.toString(), e);
        }
    }

    /**
     * Create a new job
     *
     * @param siteID
     *            site ID to use for IscScript invocations in this job.
     */
    public MosaicJob createJob() {
        return new MosaicJob();
    }

    /**
     * Submit a new job to the manager. Writes a job record to the database,
     * marked as "unprepared". Sends a notification that new work is available.
     * After calling this, clients can call {@link waitForJob} to wait for the job
     * to complete.
     *
     * @param job
     */
    public void submit(MosaicJob job) {
        IscMosaicJobRecord record = job.record;
        if (record.getSite() == null || record.getArgs() == null) {
            throw new NullPointerException("job site and args must not be null");
        }

        record.setPrepared(false);
        record.setInUse(false);
        record.setLastUse(new Date(System.currentTimeMillis()));

        synchronized (waitingJobs) {
            dao.create(record);
            waitingJobs.put(record.getId(), job);
        }
        notifyExecutors(
                new MosaicStatusMessage(MosaicStatusMessage.Type.WAKEUP));
    }

    public String waitForJob(MosaicJob job) throws InterruptedException {
        synchronized (job) {
            while (!job.done) {
                job.wait(CLUSTER_TASK_TIMEOUT / 2);
                if (!job.done && !dao.isJobPresent(job.record.getId())) {
                    job.done = true;
                    waitingJobs.remove(job.record.getId());
                    statusHandler
                            .error("did not receive a notification for completed "
                                    + getJobDescription(job.record));
                }
            }
        }
        return job.result;
    }

    /**
     * Return the collection of files to be removed when the job is completed.
     * Always returns a non-null collection.
     */
    private Collection<String> getFilesToDelete(IscMosaicJobRecord job) {
        Map<String, Object> args = null;
        try {
            args = job.getArgsMap();
        } catch (IOException e) {
            statusHandler.error("failed to get files to delete for %s: %s",
                    getJobDescription(job), e);
        }
        if (args != null) {
            Object deleteInput = args.get(DELETE_INPUT_ARG_NAME);
            if (deleteInput instanceof Boolean && (Boolean) deleteInput) {
                Object inputSpec = args.get(INPUT_FILES_ARG_NAME);
                if (inputSpec instanceof Collection) {
                    Collection<?> inputFilesCollection = (Collection<?>) inputSpec;
                    Collection<String> result = new ArrayList<>(inputFilesCollection.size());
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
     *            leader ID.  If false, just remove the given job without locking.
     */
    private void complete(IscMosaicJobRecord job, String message, Session session, boolean lockAndRemoveAll) {
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
                statusHandler.error(
                        String.format("error deleting %s: %s", path, e), e);
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
     * @param script
     * @return
     * @throws DataAccessLayerException
     */
    private boolean prepareJobs(IscScript script) throws DataAccessLayerException {
        Queue<Number> jobIDs = dao.queryJobs(false);
        Session session = dao.getSession();

        try {
            boolean didWork = false;
            while (running && !jobIDs.isEmpty()) {
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
                    script.execute(PREPARE_METHOD_NAME,
                            args, job.getSite());
                    jobsToStore = wrapper.getJobsToStore();
                } catch (Exception e) {
                    String message = String.format(
                            "iscMosaic script failed during preparation of %s: %s",
                            getJobDescription(job), e);
                    statusHandler.error(message, e);
                    complete(job, message, session, false);
                    continue;
                }
                if (jobsToStore.size() == 0) {
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
        }
    }

    /**
     * Process parms of prepared jobs.
     */
    private boolean processParms(IscScript script) throws DataAccessLayerException {
        boolean didWork = false;

        // Get prepared jobs and their parm sets
        Collection<Number> jobs = dao.queryJobs(true);
        Session session = dao.getSession();
        try {
            for (Number jobID : jobs) {
                if (! running) {
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
                List<ClusterTask> locks = ClusterLockUtils.getLocks(CLUSTER_LOCK_NAME);
                /*
                 * Sort locks in order of last execution time so that if a stale
                 * lock is overridden in @{code lockNextParm}, it will be the
                 * oldest possible lock.
                 */
                locks.sort(CT_LAST_EXEC_COMPARATOR);

                try {
                    MosaicJob jobWrapper = new MosaicJob(job);

                    while (running && !job.getParms().isEmpty()) {
                        boolean locked = processOneParm(jobWrapper, locks, session, script);
                        didWork |= locked;
                        if (! locked) {
                            /*
                             * If a cluster lock could not be obtained, do
                             * not keep looping. This will try another job,
                             * but the number of jobs is limited.
                             */
                            break;
                        }
                    }
                } finally {
                    // Call the script's cleanup method to close the NetCDF file if needed.
                    try {
                        script.execute(CLEAN_UP_JOB_METHOD_NAME, new HashMap<>(), job.getSite());
                    } catch (Exception e) {
                        String error = String.format(
                                "iscMosaic script failed during cleanup for %s: %s",
                                getJobDescription(job), e);
                        statusHandler.error(error, e);
                    }
                }

                /*
                 * If there are no more parms associated with any job record
                 * with the same leader ID as this job, complete the overall
                 * job.
                 */
                if (dao.isJobComplete(job.getLeader())) {
                    IscMosaicJobRecord leader = (IscMosaicJobRecord) session
                            .get(IscMosaicJobRecord.class, job.getLeader());
                    if (leader != null) {
                        complete(leader, null, session, true);
                    }
                }
            }
        } finally {
            session.close();
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
     * @return true if a lock was obtained (Does not imply a parm was actually
     *         processed.)
     */
    private boolean processOneParm(MosaicJob jobWrapper, List<ClusterTask> locks, Session session, IscScript script) {
        IscMosaicJobRecord job = jobWrapper.record;
        ClusterTask clusterTask = lockNextParm(job.getParms(), locks);
        try {
            if (clusterTask != null && clusterTask
                    .getLockState() == LockState.SUCCESSFUL) {
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
                    Map<String, Object> args = new HashMap<>();
                    args.put("job", jobWrapper);
                    args.put("lockName", lockName);
                    script.execute(PROCESS_PARM_METHOD_NAME, args, job.getSite());
                } catch (Exception e) {
                    String error = String.format(
                            "iscMosaic script failed for lock name %s in %s: %s",
                            lockName, getJobDescription(job), e);
                    statusHandler.error(error, e);
                }
                try {
                    dao.removeParm(job, lockName, session);
                } catch (Exception e) {
                    throw new RuntimeException(
                            String.format("error removing parm %s of %s", lockName,
                                    getJobDescription(job)),
                            e);
                }
                return true;
            } else {
                if (clusterTask != null && clusterTask
                        .getLockState() == LockState.FAILED) {
                    statusHandler.error(String.format("Attempt to take cluster lock %s failed",
                            clusterTask.getId().getDetails()));
                }
                return false;
            }
        } finally {
            if (clusterTask != null
                    && clusterTask.getLockState() == LockState.SUCCESSFUL) {
                ClusterLockUtils.unlock(clusterTask, true);
                /*
                 * Wake up other executors that may now be able
                 * to process a parm with this name.
                 */
                notifyExecutors(new MosaicStatusMessage(
                        MosaicStatusMessage.Type.WAKEUP));
            }
        }
    }

    /**
     * Work processor for the job manager. Although it implements the
     * IPythonExecutor interface, it does not actually produce a value. It will
     * run as long as {@code running} is true and the IscMosaicJobManager has
     * work.
     *
     */
    private class MosaicJobExecutor implements IPythonExecutor<IscScript, String> {

        @Override
        public String execute(IscScript script) throws JepException {
            nActiveThreads.incrementAndGet();
            try {
                int lastCounter = wakeupCounter;
                while (running) {
                    boolean didWork = false;

                    try {
                        didWork = prepareJobs(script);
                        didWork = processParms(script) || didWork;
                    } catch (Exception e) {
                        statusHandler.error(e.toString(), e);
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
                            if (running && lastCounter == wakeupCounter) {
                                try {
                                    wakeSignal.wait(CLUSTER_TASK_TIMEOUT / 2);
                                } catch (InterruptedException e) {
                                    // just continue
                                }
                            }
                            lastCounter = wakeupCounter;
                        }
                    }
                }
                return null;
            } finally {
                nActiveThreads.decrementAndGet();
                synchronized (nActiveThreads) {
                    // Wake up postStop().
                    nActiveThreads.notifyAll();
                }
            }
        }
    }

    /**
     * Attempt to lock the next parm for a job without waiting. If none of
     * the desired locks are free, attempt to override a stale lock. If no stale
     * locks can be overridden, return null.
     *
     * @param lockNames remaining lock names for parms of a job that need to be processed
     * @param locks recent list of cluster locks ordered by getLastExecution()
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
        if (ctToUse == null && fallbackCTs.size() > 0) {
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
     */
    private static void notifyExecutors(MosaicStatusMessage msg) {
        try {
            EDEXUtil.getMessageProducer().sendAsync(NOTIFY_ROUTE_NAME,
                    SerializationUtil.transformToThrift(msg));
        } catch (EdexException | SerializationException e) {
            statusHandler.error(String.format("failed to send to %s: %s",
                    NOTIFY_ROUTE_NAME, e), e);
        }
    }

    /** Called in response to an inter-process notification. */
    public void handleStatusMessage(MosaicStatusMessage msg) {
        if (msg != null
                && msg.getType() == MosaicStatusMessage.Type.COMPLETED_JOB) {
            MosaicJob job;
            synchronized (waitingJobs) {
                job = waitingJobs.remove(msg.getJobID());
            }
            if (job != null) {
                synchronized (job) {
                    job.done = true;
                    job.result = msg.getMessage();
                    job.notifyAll();
                }
            }
        } else {
            wakeUpExecutors();
        }
    }

    /**
     * Wake up executors in this proccess, starting them if they are not
     * running.
     */
    private void wakeUpExecutors() {
        synchronized (wakeSignal) {
            if (running) {
                int nToStart = nThreads - nActiveThreads.get();
                for (int i = 0; i < nToStart; ++i) {
                    this.threadPool.submitJob(new MosaicJobExecutor());
                }
                ++wakeupCounter;
                wakeSignal.notifyAll();
            }
        }
    }

    @Override
    public void preStart() {
        running = true;
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
            running = false;
            wakeSignal.notifyAll();
        }
    }

    /**
     * Wait up to @{code CLUSTER_TASK_TIMEOUT} milliseconds for worker threads
     * to terminate.
     */
    @Override
    public void postStop() {
        long start = System.nanoTime();
        long waitTime = CLUSTER_TASK_TIMEOUT * 1_000_000; // convert to nanoseconds
        synchronized (nActiveThreads) {
            while (nActiveThreads.get() > 0) {
                long left = waitTime - (System.nanoTime() - start);
                if (left <= 0) {
                    statusHandler.warn("timed out waiting for ISC mosaic jobs to finish");
                    break;
                }
                try {
                    nActiveThreads.wait(left / 1_000_000, (int)(left % 1_000_000));
                } catch (InterruptedException e) {
                    break;
                }
            }
        }
    }

}
