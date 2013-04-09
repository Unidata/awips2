package org.rzo.yajsw.os;

public interface ServiceInfo
{
	public String getName();

	public String getDisplayName();

	public String getDescription();

	public String getAccount();

	public String getCommand();

	public String getStartType();

	public String[] getDependencies();

	public boolean isInteractive();

	public int getState();

	public int getPid();

	public String getHost();

	public String getWrapped();

	public String getWrapperConfigurationPath();

	public int getWrapperJmxPort();

	public int getWrapperAppPid();

}
