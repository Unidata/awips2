package org.rzo.yajsw.wrapper;

import java.io.OutputStream;
import java.util.Date;
import java.util.logging.Logger;

import org.apache.commons.configuration.Configuration;
import org.jboss.netty.logging.InternalLogger;

public interface WrappedProcess
{
	/** The Constant STATE_IDLE. */
	static final int	STATE_IDLE			= 0;

	/** The Constant STATE_STARTING. */
	static final int	STATE_STARTING		= 1;

	/** The Constant STATE_RESTART. */
	static final int	STATE_RESTART		= 2;

	/** The Constant STATE_STOP. */
	static final int	STATE_STOP			= 3;

	/** The Constant STATE_RUNNING. */
	static final int	STATE_RUNNING		= 4;

	/** The Constant STATE_RESTART_START. */
	static final int	STATE_RESTART_START	= 5;

	/** The Constant STATE_RESTART_STOP. */
	static final int	STATE_RESTART_STOP	= 6;

	/** The Constant STATE_RESTART_WAIT. */
	static final int	STATE_RESTART_WAIT	= 7;

	/** The Constant STATE_USER_STOP. */
	static final int	STATE_USER_STOP		= 8;

	/** The Constant STATE_ABORT. */
	static final int	STATE_ABORT			= 9;

	static final int	STATE_SHUTDOWN		= 10;

	// used only within tray icon to show that app has not yet reported that it is up and running
	static final int	STATE_APP_WAIT	= 11;

	/**
	 * Inits the.
	 */
	public void init();

	/**
	 * Start.
	 */
	public void start();

	/**
	 * Stop.
	 */
	public void stop();
	public void stop(String reason);

	/**
	 * Restart.
	 */
	public void restart();

	/**
	 * Gets the pid.
	 * 
	 * @return the pid
	 */
	public int getAppPid();

	/**
	 * Reconnect.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return true, if successful
	 */
	public boolean reconnect(int pid);

	/**
	 * Gets the local configuration.
	 * 
	 * @return the local configuration
	 */
	public Configuration getLocalConfiguration();

	/**
	 * Gets the exit code.
	 * 
	 * @return the exit code
	 */
	public int getExitCode();

	/**
	 * Sets the use system properties.
	 * 
	 * @param useSystemProperties
	 *            the new use system properties
	 */
	public void setUseSystemProperties(boolean useSystemProperties);

	public int getState();

	public void stopTimer();

	public void restartInternal();

	public void startByTimer();

	public void restartByTimer();

	public void setDebug(boolean b);

	public void addStateChangeListener(int state, StateChangeListener listener);

	public void addStateChangeListener(StateChangeListener listener);

	public void addTriggerListener(TriggerListener listener);

	public int getRestartCount();

	public String getStringState();

	public String getName();

	public void startDrain();

	public String readDrainLine();

	public void stopDrain();

	public OutputStream getOutputStream();

	public Date getAppStarted();

	public Date getAppStopped();

	public Date getWrapperStarted();

	public int getAppThreads();

	public long getAppVMemory();
	public long getAppPMemory();

	public int getAppCpu();

	public int getAppHandles();

	public int getWrapperPid();

	public boolean isTimerActive();

	public boolean isConditionActive();

	public void threadDump();

	public void gc();
	
	public void dumpHeap(String fileName);

	public void stopTimerCondition();

	public boolean isOSProcessRunning();

	public void waitFor();

	public void waitFor(long duration);

	public int getTotalRestartCount();

	public String getType();

	public Logger getWrapperLogger();

	public void removeStateChangeListener(StateChangeListener listener);

	public void shutdown();

	public void setLocalConfiguration(Configuration config);

	public void osProcessTerminated();

	public boolean isHaltWrapperOnApp();

	public boolean isHaltAppOnWrapper();

	public void removeStateChangeListener(int state);

	public void setExiting();

	public boolean isExiting();

	public TrayIconProxy getTrayIcon();

	public void setService(Object service);

	public long getMaxStartTime();

	public void setStopper(boolean b);
	
	public Configuration getYajswConfig();

	public void signalStopping(long valueOf);
	
	public boolean isAppReportedReady();

	public void setAppReportedReady(boolean appReportedReady);

	public InternalLogger getInternalWrapperLogger();
	
	public void update(String conf, boolean autostart);


}