package org.rzo.yajsw.os;

import java.util.logging.Logger;

import org.apache.commons.configuration.Configuration;

public interface Service
{
	public static final int	STATE_INSTALLED		= 1;
	public static final int	STATE_RUNNING		= 2;
	public static final int	STATE_INTERACTIVE	= 4;
	public static final int	STATE_AUTOMATIC		= 8;
	public static final int	STATE_MANUAL		= 16;
	public static final int	STATE_DISABLED		= 32;
	public static final int	STATE_PAUSED		= 64;

	public static final int	STATE_STARTING		= 48;
	public static final int	STATE_STOPPING		= 40;

	public static final int	STATE_UNKNOWN		= 128;

	public boolean start();

	public boolean stop();

	public boolean install();

	public boolean uninstall();

	public boolean isInstalled(int state);

	public boolean isRunning(int state);

	public boolean isInteractive(int state);

	public boolean isAutomatic(int state);

	public boolean isManual(int state);

	public boolean isDisabled(int state);

	public boolean isPaused(int state);

	public boolean isStateUnknown(int state);

	public int state();

	public String getName();

	public void setName(String name);

	public Configuration getConfig();

	public void setConfig(Configuration config);

	public String getDisplayName();

	public void setDisplayName(String displayName);

	public String getDescription();

	public void setDescription(String description);

	public String[] getDependencies();

	public void setDependencies(String[] dependencies);

	public String getAccount();

	public void setAccount(String account);

	public String getPassword();

	public void setPassword(String password);

	public String[] getCommand();

	public void setCommand(String[] command);

	public void init();

	public void setStartType(String startType);

	public String getStartType();

	public boolean isInteractive();

	public void setInteractive(boolean interactive);

	public void setLogger(Logger log);

	public boolean isStarting(int state);
	
	public void setFailureActions(Object failureActions);
	
	public Object getFailureActions();

}
