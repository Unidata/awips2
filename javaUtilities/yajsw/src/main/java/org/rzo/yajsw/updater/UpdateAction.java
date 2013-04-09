package org.rzo.yajsw.updater;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;

public class UpdateAction
{
	static String _updateConfigFile;
	static YajswConfigurationImpl _currentConfig;
	static Process _p;
	static boolean _autoStart = false;
	
	public static void setUpdateConfig(String updateConfigFile)
	{
		_updateConfigFile = updateConfigFile;
	}

	public static void setCurrentConfig(YajswConfigurationImpl config)
	{
		_currentConfig = config;
	}
	
	public static void setAutostart()
	{
		_autoStart = true;
	}

	public static void run()
	{
		try
		{
		if (_p != null)
			return;
		if (_updateConfigFile == null || _updateConfigFile.length() == 0)
			return;
		_p = OperatingSystem.instance().processManagerInstance().createProcess();
		_p.setCommand(getUpdaterCommand());
		// daemonize !
		_p.setPipeStreams(false, false);
		_p.setVisible(false);
		// set working dir to wrapper home, so we know where we are running
		// not required, but good
		_p.setWorkingDir(WrapperLoader.getWrapperHome());
		_p.start();
		// wait, so that the service has been uninstalled and thus could not be restarted
		// until it has been reinstalled
		_p.waitFor(10000);
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
		}
	}

	private static String[] getUpdaterCommand()
	{
		List<String> result = new ArrayList<String>();
		// set java
		JavaHome javaHome = OperatingSystem.instance().getJavaHome(_currentConfig);
		String java = javaHome.findJava(_currentConfig.getString("wrapper.java.command"), _currentConfig.getString("wrapper.java.customProcName"));
		result.add(java);
		// set classpath
		result.add("-classpath");
		result.add(WrapperLoader.getWrapperJar()+File.pathSeparator+WrapperLoader.getWrapperAppJar());
		// forward system props to sub process
		for (Object obj : System.getProperties().keySet())
		{
			String key = (String)obj;
			result.add("-D"+key+"="+System.getProperty(key));
		}
		if (_autoStart)
			result.add("-Dupdate.autostart=true");
		// set main class
		result.add(UpdaterBooter.class.getName());
		// set main class args - update config file
		result.add(_updateConfigFile);
		result.add(new File(_currentConfig.getCachedPath(false)).getAbsolutePath());
		String[] arrResult = new String[result.size()];
		for (int i=0; i< arrResult.length; i++)
			arrResult[i] = result.get(i);
		return arrResult;
	}
	
	// test
	public static void main(String[] args)
	{
		UpdateAction.setUpdateConfig("z:/dev/yajsw/update.conf");
		System.setProperty("wrapper.config", "z:/dev/yajsw/conf/wrapper.helloworld.conf");
		YajswConfigurationImpl conf = new YajswConfigurationImpl();
		UpdateAction.setCurrentConfig(conf);
		System.out.println("service "+conf.getString("wrapper.ntservice.name", "?"));
		UpdateAction.run();
	}

}
