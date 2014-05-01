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

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Object that encapsulates the status of a plugin purge job
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Table(name = "purgejobs")
public class PurgeJobStatus implements Serializable, ISerializableObject,
		Cloneable {

	private static final long serialVersionUID = -6381471022735050985L;

	/** The plugin that is to be purged by this job */
	@Id
	@Column(nullable = false, length = 64)
	private String plugin;

	/** Denotes if this job is currently running */
	@Column(nullable = false)
	private boolean running = false;

	/** The time that this purger started execution */
	@Column(nullable = false)
	private Date startTime;

	/**
	 * The number of consecutive times this purge has failed to execute
	 * successfully
	 */
	@Column(nullable = false)
	private int failedCount;

	public PurgeJobStatus() {

	}

	/**
	 * Creates a new PurgeJobStatus with the given values
	 * 
	 * @param plugin
	 * @param running
	 * @param startTime
	 * @param failedCount
	 */
	public PurgeJobStatus(String plugin, boolean running, Date startTime,
			int failedCount) {
		this.plugin = plugin;
		this.running = running;
		this.startTime = startTime;
		this.failedCount = failedCount;
	}

	/**
	 * Gets how long this job has been running in milliseconds
	 * 
	 * @return The execution duration
	 */
	public long getRunningTime() {
		return System.currentTimeMillis() - startTime.getTime();
	}

	/**
	 * Gets how long this job has been running in minutes
	 * 
	 * @return The execution duration in minutes
	 */
	public long getRunningTimeAsMinutes() {
		return getRunningTime() / 60000;
	}

	/**
	 * Increments the failed count
	 */
	public void incrementFailedCount() {
		this.failedCount++;
	}

	public String getPlugin() {
		return plugin;
	}

	public void setPlugin(String plugin) {
		this.plugin = plugin;
	}

	public boolean isRunning() {
		return running;
	}

	public void setRunning(boolean running) {
		this.running = running;
	}

	public Date getStartTime() {
		return startTime;
	}

	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	public int getFailedCount() {
		return failedCount;
	}

	public void setFailedCount(int failedCount) {
		this.failedCount = failedCount;
	}

	@Override
	public Object clone() {
		return new PurgeJobStatus(plugin, running, startTime, failedCount);
	}
}
