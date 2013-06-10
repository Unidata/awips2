package org.rzo.yajsw.updater;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;


public class InstallAction
{


		static YajswConfigurationImpl _currentConfig;
		static String _newConfig;
		static String _wrapperHome;
		static Process _p;
		
		public static void setWrapperHome(String wrapperHome)
		{
			_wrapperHome = wrapperHome;
		}

		public static void setCurrentConfig(YajswConfigurationImpl config)
		{
			_currentConfig = config;
		}
		
		public static void setNewConfig(String newConfig)
		{
			_newConfig = newConfig;
		}

		public static void run()
		{
			try
			{
			if (_p != null)
				return;
			if (_currentConfig == null || _newConfig == null || _wrapperHome == null)
				return;
			_p = OperatingSystem.instance().processManagerInstance().createProcess();
			_p.setCommand(getInstallerCommand());
			// daemonize !
			_p.setPipeStreams(false, false);
			_p.setVisible(false);
			// set working dir to wrapper home, so we know where we are running
			// not required, but good
			_p.setWorkingDir(WrapperLoader.getWrapperHome());
			_p.start();
			}
			catch (Throwable ex)
			{
				ex.printStackTrace();
			}
		}

		private static String[] getInstallerCommand()
		{
			List<String> result = new ArrayList<String>();
			// set java
			JavaHome javaHome = OperatingSystem.instance().getJavaHome(_currentConfig);
			String java = javaHome.findJava(_currentConfig.getString("wrapper.java.command"), _currentConfig.getString("wrapper.java.customProcName"));
			result.add(java);
			// set classpath
			result.add("-classpath");
			result.add(getWrapperJar()+File.pathSeparator+getWrapperAppJar());
			// forward system props to sub process
			for (Object obj : System.getProperties().keySet())
			{
				String key = (String)obj;
				result.add("-D"+key+"="+System.getProperty(key));
			}
			// set main class
			result.add(InstallerBooter.class.getName());
			// set main class args - current.conf new.conf
			result.add(new File(_currentConfig.getCachedPath(false)).getAbsolutePath());
			result.add(_newConfig);
			String[] arrResult = new String[result.size()];
			for (int i=0; i< arrResult.length; i++)
				arrResult[i] = result.get(i);
			return arrResult;
		}
		
		private static String getWrapperJar()
		{
			return _wrapperHome+File.separator+"wrapper.jar";
		}

		private static String getWrapperAppJar()
		{
			return _wrapperHome+File.separator+"wrapperApp.jar";
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
