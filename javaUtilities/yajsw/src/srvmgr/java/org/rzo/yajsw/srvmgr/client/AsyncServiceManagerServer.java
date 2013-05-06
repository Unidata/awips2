package org.rzo.yajsw.srvmgr.client;



public interface AsyncServiceManagerServer
{
	public Object getServiceList();
	public Object getService(String name);
	public Object start(String name);
	public Object stop(String name);
	public Object yajswInstall(String configuration);
	public Object yajswUninstall(String name);
	public Object yajswReloadConsole(String name, String newConfig);
	public Object isServiceManager();

}
