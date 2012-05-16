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
package com.raytheon.uf.edex.purgesrv;

import java.lang.Thread.State;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.status.StatusConstants;
import com.raytheon.uf.edex.purgesrv.PurgeJob.PURGE_JOB_TYPE;

/**
 * 
 * Object for managing purge jobs. The purge manager relies on the purgejobs
 * table to coordinate information. The executePurge() method on this class is
 * executed every minute via a quartz timer defined in the purge-spring.xml
 * Spring configuration file.
 * <p>
 * The purge manager is designed to adhere to the following rules:
 * <p>
 * · The cluster may have no more than 6 purge jobs running simultaneously by
 * default. This property is configurable in the project.properties file<br>
 * · Any given server may have no more than 2 purge jobs running simultaneously
 * by default. This property is configurable in the project.properties file<br>
 * · A purge job for a plugin is considered 'hung' if it has been running for
 * more than 20 minutes by default. This property is configurable in the
 * project.properties file <br>
 * · If a purge job that was previously determined to be hung actually finishes
 * it's execution, the cluster lock is updated appropriately and the purge job
 * is able to resume normal operation. This is in place so if a hung purge
 * process goes unnoticed for a period of time, the server will still try to
 * recover autonomously if it can. <br>
 * · If a purge job is determined to be hung, the stack trace for the thread
 * executing the job is output to the log. Furthermore, if the job is in the
 * BLOCKED state, the stack traces for all other BLOCKED threads is output to
 * the purge log as part of a rudimentary deadlock detection strategy to be used
 * by personnel attempting to remedy the situation.<br>
 * · By default, a fatal condition occurs if a given plugin's purge job fails 3
 * consecutive times.<br>
 * · If a purge job hangs on one server in the cluster, it will try and run on
 * another cluster member at the next purge interval.<br>
 * · If the purge manager attempts to purge a plugin that has been running for
 * longer than the 20 minute threshold, it is considered a failure, and the
 * failure count is updated.
 * <p>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2012 #470       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class PurgeManager {

	/** Purge Manager task name */
	private static final String PURGE_TASK_NAME = "Purge Manager";

	/** Purge Manager task details */
	private static final String PURGE_TASK_DETAILS = "Purge Manager Job";

	/** Purge Manager task override timeout. Currently 2 minutes */
	private static final long PURGE_MANAGER_TIMEOUT = 120000;

	/**
	 * The cluster limit property to be set via Spring with the value defined in
	 * project.properties
	 */
	private int clusterLimit = 6;

	/**
	 * The server limit property to be set via Spring with the value defined in
	 * project.properties
	 */
	private int serverLimit = 2;

	/**
	 * The time in minutes at which a purge job is considered 'dead' or 'hung'
	 * set via Spring with the value defined in project.properties
	 */
	private int deadPurgeJobAge = 20;

	/**
	 * The frequency, in minutes, that a plugin may be purged set via Spring
	 * with the value defined in project.properties
	 */
	private int purgeFrequency = 60;

	/**
	 * How many times a purger is allowed to fail before it is considered fatal.
	 * Set via Spring with the value defined in project.properties
	 */
	private int fatalFailureCount = 3;

	/**
	 * The master switch defined in project.properties that enables and disables
	 * data purging
	 */
	private boolean purgeEnabled = true;

	/** Map of purge jobs */
	private Map<String, PurgeJob> purgeJobs = new ConcurrentHashMap<String, PurgeJob>();

	private PurgeDao dao = new PurgeDao();

	private static PurgeManager instance = new PurgeManager();

	public static PurgeManager getInstance() {
		return instance;
	}

	/**
	 * Creates a new PurgeManager
	 */
	private PurgeManager() {
	}

	/**
	 * Executes the purge routine
	 */
	public void executePurge() {
		if (!purgeEnabled) {
			PurgeLogger.logWarn(
					"Data purging has been disabled.  No data will be purged.",
					null);
			return;
		}

		ClusterTask purgeMgrTask = getPurgeLock();

		try {
			// Prune the job map
			Iterator<PurgeJob> iter = purgeJobs.values().iterator();
			while (iter.hasNext()) {
				if (!iter.next().isAlive()) {
					iter.remove();
				}
			}
			Calendar purgeTimeOutLimit = Calendar.getInstance();
			purgeTimeOutLimit.setTimeZone(TimeZone.getTimeZone("GMT"));
			purgeTimeOutLimit.add(Calendar.MINUTE, -deadPurgeJobAge);
			Calendar purgeFrequencyLimit = Calendar.getInstance();
			purgeFrequencyLimit.setTimeZone(TimeZone.getTimeZone("GMT"));
			purgeFrequencyLimit.add(Calendar.MINUTE, -purgeFrequency);

			// Gets the list of plugins in ascending order by the last time they
			// were purged
			List<String> pluginList = dao.getPluginsByPurgeTime();

			// check for any new plugins or database being purged and needing
			// entries recreated
			Set<String> availablePlugins = new HashSet<String>(PluginRegistry
					.getInstance().getRegisteredObjects());

			// Merge the lists
			availablePlugins.removeAll(pluginList);

			if (availablePlugins.size() > 0) {
				// generate new list with them at the beginning
				List<String> newSortedPlugins = new ArrayList<String>(
						availablePlugins);
				Collections.sort(newSortedPlugins);
				newSortedPlugins.addAll(pluginList);
				pluginList = newSortedPlugins;
			}

			boolean canPurge = true;
			int jobsStarted = 0;
			int maxNumberOfJobsToStart = Math.min(
					clusterLimit
							- dao.getRunningClusterJobs(
									purgeTimeOutLimit.getTime(),
									fatalFailureCount), serverLimit
							- getNumberRunningJobsOnServer(purgeTimeOutLimit));
			for (String plugin : pluginList) {
				try {
					// initialize canPurge based on number of jobs started
					canPurge = jobsStarted < maxNumberOfJobsToStart;
					PurgeJob jobThread = purgeJobs.get(plugin);
					PurgeJobStatus job = dao.getJobForPlugin(plugin);

					if (job == null) {
						// no job on server, generate empty job

						try {
							job = new PurgeJobStatus();
							job.setPlugin(plugin);
							job.setFailedCount(0);
							job.setRunning(false);
							job.setStartTime(new Date(0));
							dao.create(job);
						} catch (Throwable e) {
							PurgeLogger.logError(
									"Failed to create new purge job entry",
									plugin, e);
						}
					}

					// Check to see if this job has met the fatal failure count
					if (job.getFailedCount() >= fatalFailureCount) {
						canPurge = false;
						PurgeLogger
								.logFatal(
										"Purger for this plugin has reached or exceeded consecutive failure limit of "
												+ fatalFailureCount
												+ ".  Data will no longer being purged for this plugin.",
										plugin);
					}

					if (job.isRunning()) {
						// check if job has timed out
						if (purgeTimeOutLimit.getTimeInMillis() > job
								.getStartTime().getTime()) {
							// if no one else sets canPurge = false will start
							// purging on this server
							if (jobThread != null) {
								// job currently running on our server, don't
								// start another
								canPurge = false;
								jobThread.printTimedOutMessage(deadPurgeJobAge);
							}
						}
					} else {
						// not currently running, check if need to be purged
						Date startTime = job.getStartTime();
						if (startTime != null
								&& startTime.getTime() >= purgeFrequencyLimit
										.getTimeInMillis()) {
							canPurge = false;
						}
					}

					if (canPurge) {
						purgeJobs.put(plugin, purgeExpiredData(plugin));
						jobsStarted++;
					}
				} catch (Throwable e) {
					PurgeLogger
							.logError(
									"An unexpected error occured during the purge job check for plugin",
									plugin, e);
				}
			}
		} catch (Throwable e) {
			PurgeLogger
					.logError(
							"An unexpected error occured during the data purge process",
							StatusConstants.CATEGORY_PURGE, e);
		} finally {
			// Unlock the purge task to allow other servers to run.
			ClusterLockUtils.unlock(purgeMgrTask, false);
			// PurgeLogger.logInfo(getPurgeStatus(true), null);
		}
	}

	@SuppressWarnings("unused")
	private String getPurgeStatus(boolean verbose) {
		Calendar purgeTimeOutLimit = Calendar.getInstance();
		purgeTimeOutLimit.setTimeZone(TimeZone.getTimeZone("GMT"));
		purgeTimeOutLimit.add(Calendar.MINUTE, -deadPurgeJobAge);

		StringBuilder builder = new StringBuilder();
		List<PurgeJobStatus> failedJobs = dao.getFailedJobs(fatalFailureCount);

		List<PurgeJobStatus> timedOutJobs = dao
				.getTimedOutJobs(purgeTimeOutLimit.getTime());
		int clusterJobs = dao.getRunningClusterJobs(
				purgeTimeOutLimit.getTime(), fatalFailureCount);
		Map<String, List<PurgeJobStatus>> serverMap = dao
				.getRunningServerJobs();
		builder.append("\nPURGE JOB STATUS:");
		builder.append("\n\tTotal Jobs Running On Cluster: ").append(
				clusterJobs);
		List<PurgeJobStatus> jobs = null;
		for (String server : serverMap.keySet()) {
			jobs = serverMap.get(server);
			builder.append("\n\tJobs Running On ").append(server).append(": ")
					.append(jobs.size());
			if (verbose && !jobs.isEmpty()) {
				builder.append("   Plugins: ");
				for (int i = 0; i < jobs.size(); i++) {
					builder.append(jobs.get(i).getPlugin());
					if (i != jobs.size() - 1) {
						builder.append(",");
					}
				}
			}
		}
		if (verbose) {
			builder.append("\n\tFailed Jobs: ");
			if (failedJobs.isEmpty()) {
				builder.append("0");
			} else {
				PurgeJobStatus currentJob = null;
				for (int i = 0; i < failedJobs.size(); i++) {
					currentJob = failedJobs.get(i);
					builder.append(currentJob.getPlugin());
					if (i != failedJobs.size() - 1) {
						builder.append(",");
					}
				}
			}

			builder.append("\n\tTimed Out Jobs: ");
			if (timedOutJobs.isEmpty()) {
				builder.append("0");
			} else {
				PurgeJobStatus currentJob = null;
				for (int i = 0; i < timedOutJobs.size(); i++) {
					currentJob = timedOutJobs.get(i);
					builder.append(currentJob.getPlugin());
					if (i != timedOutJobs.size() - 1) {
						builder.append(",");
					}
				}
			}
		}
		return builder.toString();
	}

	public ClusterTask getPurgeLock() {
		// Lock so only one cluster member may start purge processes
		ClusterTask purgeMgrTask = ClusterLockUtils.lock(PURGE_TASK_NAME,
				PURGE_TASK_DETAILS, PURGE_MANAGER_TIMEOUT, true);

		LockState purgeMgrLockState = purgeMgrTask.getLockState();
		switch (purgeMgrLockState) {
		case FAILED:
			PurgeLogger.logError(
					"Purge Manager failed to acquire cluster task lock",
					StatusConstants.CATEGORY_PURGE);
			return null;
		case OLD:
			PurgeLogger.logWarn("Purge Manager acquired old cluster task lock",
					StatusConstants.CATEGORY_PURGE);
			break;
		case ALREADY_RUNNING:
			PurgeLogger
					.logWarn(
							"Purge Manager acquired currently running cluster task lock",
							StatusConstants.CATEGORY_PURGE);
			return null;
		case SUCCESSFUL:
			break;
		}
		return purgeMgrTask;
	}

	private int getNumberRunningJobsOnServer(Calendar timeOutTime) {
		int rval = 0;
		for (PurgeJob job : purgeJobs.values()) {
			// if job has not timed out or if the job is not blocked consider it
			// running on this server
			if (timeOutTime.getTimeInMillis() < job.getStartTime()
					|| !job.getState().equals(State.BLOCKED)) {
				rval++;
			}

		}
		return rval;
	}

	/**
	 * Starts a purge expired data job for the specified plugin. Using this
	 * method allows for exceeding failure count via a manual purge as well as
	 * kicking off a second purge for one already running on a server.
	 * 
	 * @param plugin
	 *            The plugin to purge the expired data for
	 * @return The PurgeJob that was started
	 */
	public PurgeJob purgeExpiredData(String plugin) {
		dao.startJob(plugin);
		PurgeJob job = new PurgeJob(plugin, PURGE_JOB_TYPE.PURGE_EXPIRED);
		job.start();
		return job;
	}

	/**
	 * Starts a purge all data job for the specified plugin. Using this method
	 * allows for exceeding failure count via a manual purge as well as kicking
	 * off a second purge for one already running on a server.
	 * 
	 * @param plugin
	 *            The plugin to purge all data for
	 * @return The PurgeJob that was started
	 */
	public PurgeJob purgeAllData(String plugin) {
		dao.startJob(plugin);
		PurgeJob job = new PurgeJob(plugin, PURGE_JOB_TYPE.PURGE_ALL);
		job.start();
		return job;
	}

	public int getClusterLimit() {
		return clusterLimit;
	}

	public void setClusterLimit(int clusterLimit) {
		this.clusterLimit = clusterLimit;
	}

	public int getServerLimit() {
		return serverLimit;
	}

	public void setServerLimit(int serverLimit) {
		this.serverLimit = serverLimit;
	}

	public int getDeadPurgeJobAge() {
		return deadPurgeJobAge;
	}

	public void setDeadPurgeJobAge(int deadPurgeJobAge) {
		this.deadPurgeJobAge = deadPurgeJobAge;
	}

	public int getPurgeFrequency() {
		return purgeFrequency;
	}

	public void setPurgeFrequency(int purgeFrequency) {
		this.purgeFrequency = purgeFrequency;
	}

	public int getFatalFailureCount() {
		return this.fatalFailureCount;
	}

	public void setFatalFailureCount(int fatalFailureCount) {
		this.fatalFailureCount = fatalFailureCount;
	}

	public void setPurgeEnabled(boolean purgeEnabled) {
		this.purgeEnabled = purgeEnabled;
	}

	public boolean getPurgeEnabled() {
		return purgeEnabled;
	}
}
